# Node Health Detection

A cluster node whose networking is partially degraded - a host or uplink dropping a large fraction of packets in both directions - can be hard to spot. It stays "up" (it answers, participates in elections, has normal CPU and memory), yet it silently fails to keep queues synchronised and drags on the cluster. Reachability-based health checks miss it because the node is reachable, just lossy.

This feature turns the signal the broker already has - the node failure detector's per-peer reachability estimate - into per-node metrics an alarm can act on, so a degraded node can be attributed and replaced early. It is **off by default**.

## What it does

Each node continuously estimates, for every peer, a probability in `[0.0, 1.0]` that the peer is down (this comes from `aten`, the accrual failure detector that ships with Ra). A single node only has its own row of that picture. The nodes gossip their rows to each other so that each node holds the full observer-by-peer matrix, keeps a bounded rolling window of it, and decides one of:

- `clean` - no node stands out.
- `{suspect, Node}` - one node is degraded and attributable.
- `cluster_wide` - the degradation is symmetric across the cluster, so it is not blamed on any single node.

The decision is exposed as Prometheus gauges (below); nothing is acted on inside the broker.

## Metrics

All are gauges scraped from the standard `rabbitmq_prometheus` endpoint. The first three are labelled by `peer`; the fourth is a cluster-level signal with no label:

| Metric | Meaning |
|---|---|
| `rabbitmq_aws_node_health_peer_down_probability{peer}` | This node's own raw estimate that `peer` is down. One row of the matrix; needs no gossip. |
| `rabbitmq_aws_node_health_peer_down_suspected{peer}` | `1` if the cross-node decision attributes `peer` as the single degraded node, else `0`. |
| `rabbitmq_aws_node_health_peer_down_confidence{peer}` | Confidence in `[0.0, 1.0]` that `peer` is the single degraded node. `0.0` whenever the node is not suspected. |
| `rabbitmq_aws_node_health_cluster_congested` | `1` if the degradation is symmetric across the cluster (`cluster_wide` verdict), i.e. congestion not attributable to any single node, else `0`. No `peer` label. |

A dumb alarm can watch `rabbitmq_aws_node_health_peer_down_suspected` (fire if any node reports a peer suspected for long enough) or threshold `rabbitmq_aws_node_health_peer_down_confidence`. `rabbitmq_aws_node_health_cluster_congested` is the complementary signal: it fires when *every* node is elevated with no dominant culprit, which is the case `suspected` deliberately does not attribute. `rabbitmq_aws_node_health_peer_down_probability` is the raw underlay, useful for dashboards and for confirming the decision.

The suspected/confidence values are the same across all healthy nodes because they share the matrix, so an alarm sees corroborating reports from the peers of the degraded node. A node never reports itself: the three per-peer gauges cover only a node's peers, so `suspected`/`confidence` share the same `peer` domain as `probability`. `rabbitmq_aws_node_health_cluster_congested` is likewise identical across healthy nodes (they share the matrix), so any one node's value suffices.

Both `rabbitmq_aws_node_health_peer_down_suspected` and `rabbitmq_aws_node_health_cluster_congested` are debounced by the same `confirm_ticks`/`clear_ticks` hysteresis (see below): each is asserted only after its verdict has held for `confirm_ticks` consecutive cycles and de-asserted only after `clear_ticks` consecutive cycles without it. A confirmed single-node suspect takes precedence over `cluster_wide`, so the two never both read `1` at once.

## How the decision is made

For each node the detector computes an inbound score - the median of the *other* nodes' views of it - over the window, and a flap count - the number of upward crossings of the extreme threshold in its inbound series. The three paths plus a guard are evaluated in this order: **P2, then P3, then the cluster-wide guard, then P1.** The two dominant-single-node paths (P2 and P3) run *before* the guard, so a node that clearly dominates is still attributed even when a background peer is also elevated; the guard then catches the genuinely symmetric case, and the weaker P1 flap path is considered only if nothing above matched. Attribution runs only with three or more observed nodes; below that the cross-check majority the algorithm relies on is not available, and the verdict is unconditionally `clean`.

- **P2, sustained fault.** The candidate (highest inbound median) is extreme for most of the window and leads the next node by a wide margin. This covers a continuous fault that pins the probability high (verified in vivo: continuous ~48% loss keeps the failure-detector probability pinned near 1.0, it does not re-normalise downward) even when other nodes are mildly elevated by background loss.
- **P3, masked fault (bidirectional).** The candidate (highest inbound median) is elevated inbound *and* its own outbound row shows every peer degraded, *and* that own-outbound dominates every other node's by a margin. A real single-node network fault is bidirectional - the faulty node's lost ACKs stall its inbound too, so it sees all peers degraded - whereas a healthy node elevated only by cluster-wide congestion still sees its own peers roughly normally. This attributes a masked fault (a single node worse than a congested background) that P2's margin misses. The dominance requirement is what keeps P3 out under *uniform* congestion, where every node is roughly equally bidirectional so none dominates and the verdict stays `cluster_wide`. P3 also requires every *other* node to have a usable own gossip row in the window, one that yields an own-outbound value: dominance across zeros - an absent row, or a present but empty one from a just-restarted node - is not real dominance, so such a row disqualifies the comparison.
- **Cluster-wide guard.** If two or more nodes are elevated with no dominant culprit (P2/P3 both failed to fire), the condition is symmetric (congestion, not a single fault) and the verdict is `cluster_wide` - no node is blamed.
- **P1, isolated fault (flap-rate).** P1 selects its candidate by *flap count* (not median): the node whose inbound series crosses the extreme threshold the most times. An intermittent, low-duty-cycle fault has a near-zero median (mostly-zero series with brief high spikes), so choosing by median would name a different, quiet node and count no flaps on the actual culprit; choosing by flap count catches this case directly. P1 fires when the flap candidate has at least `flap_min` crossings *and* every other node is pristine on both signals (median below `pristine` and flap count below `flap_min`), so only a truly isolated flapper is attributed.

The "every other node pristine" gate on P1, and P3's dominance-with-present-others requirement, are what prevent a false positive under cluster-wide congestion, where a node can read elevated inbound without any real fault.

**Hysteresis.** The per-tick verdict is intentionally noisy (under congestion a healthy node can momentarily win the candidate slot), so the published `suspected` flag is debounced: a node is only marked suspected after it has been the raw suspect for `confirm_ticks` consecutive cycles, and stays suspected until it has *not* been the raw suspect for `clear_ticks` consecutive cycles. This applies uniformly to all three paths.

Attribution needs at least three observed nodes (a two-node view cannot tell which side of a link is bad), so below that the detector returns `clean` rather than guess; Amazon MQ clusters are three-node. It relies on the healthy nodes' views of the suspect, so the suspect's own (unreliable) gossip is not required.

## Configuration

All keys are optional; the feature is disabled unless the toggle is set.

| Key | Default | Meaning |
|---|---|---|
| `aws.node_health.enabled` | `false` | Main toggle. When off, no worker runs and no metrics are registered. |
| `aws.node_health.interval_ms` | `1000` | Sampling and recompute period. |
| `aws.node_health.window` | `30` | Number of snapshots in the rolling decision window. |
| `aws.node_health.stale_ticks` | `5` | Drop a peer's row if it has not refreshed within this many ticks. |
| `aws.node_health.confirm_ticks` | `3` | Hysteresis: consecutive cycles a node must be the raw suspect before `suspected` is published. |
| `aws.node_health.clear_ticks` | `3` | Hysteresis: consecutive cycles without being the raw suspect before a published suspect clears. |

Example:

```
aws.node_health.enabled = true
```

## Limitations

- Cluster-wide network congestion is not attributed to a node - by design, since it is not a single-node fault. It is instead surfaced on its own via `rabbitmq_aws_node_health_cluster_congested`, so an operator can still alarm on symmetric congestion without misattributing it.
- The signal measures Erlang-distribution reachability, so it identifies the degraded *node*, not the underlying cause (a GC/CPU stall on a node looks similar to a network fault).
- Gossip relies on the healthy nodes being able to exchange small rows; a fully partitioned node is already covered by the coarser detectors (`net_tick`, cluster partition handling).
- The underlying failure detector reports, for each peer, the fraction of recent inter-arrival gaps that are small relative to the current silence. Under an *intermittent* fault this oscillates (it falls back during the clean intervals), which is why P1 uses flap-rate. Under a *continuous* fault it stays pinned high (verified in vivo at ~48% loss - the probability does *not* re-normalise downward, because most heartbeats still arrive on time over TCP and only a heavy tail stalls), which is why P2 covers the sustained case. The `window`, `flap_min`, and crossing-threshold defaults are validated in vivo for both the periodic (intermittent-drop) pattern via P1 and continuous loss via P2 - in each case the verdict fires within ~15-20s and holds for the duration of the fault, with the healthy nodes never falsely attributed - and may warrant tuning for a given cluster's fault profile.
- Under sustained *symmetric* congestion, a node with a structural asymmetry (more queue leaders, a load-balancer target) reads a higher inbound probability than its peers, and when its lead crosses P2's `margin` the P2 path can transiently attribute it as a single-node suspect instead of reporting `cluster_wide`. P2 evaluates before the cluster-wide guard and, unlike P3, has no own-outbound dominance check, so bidirectionality (which under uniform congestion is present on every node anyway) does not screen it out. This is a false positive on a healthy node, not a missed fault; it is intermittent (a boundary effect at the margin) and was observed in vivo under uniform latency+jitter but not under uniform loss. Tracked in [issue #191](https://github.com/amazon-mq/rabbitmq-aws/issues/191).

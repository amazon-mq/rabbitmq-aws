# Node Health Detection: Testing

How the node-health detector is tested, at two levels: automated suites that pin the scorer, worker, collector, and config, and an in-vivo procedure that validates the real failure-detector signal and the end-to-end behaviour on a live three-node cluster. See [NODE_HEALTH.md](NODE_HEALTH.md) for the feature itself.

## Automated tests

- `test/aws_node_health_tests.erl` (eunit) - the pure scorer against captured probability fixtures and synthetic windows: every path (P1, P2, P3), the cluster-wide guard, hysteresis, the sub-three-node guard, and the continuous-loss onset-to-sustained hand-off. Each hand-off case asserts the path-specific confidence value (P2's extreme fraction, P1's flap ratio), which fingerprints the attributing path through the public result even though `analyze/2` does not return it.
- `test/prop_aws_node_health_SUITE.erl` (property-based) - invariants over randomly generated matrices: the scorer never crashes, verdict and scores never disagree, a symmetric matrix is never attributed, and below three nodes the verdict is `clean`.
- `test/aws_node_health_worker_tests.erl`, `test/aws_node_health_metrics_tests.erl`, `test/aws_node_health_config_tests.erl` (eunit) - row validation and stale-row pruning, the collector's sample builders, and the config reads and bounds.
- `test/aws_node_health_SUITE.erl` (Common Test) - boots a real broker, lets `aws_sup` start the worker and register the collector from config, and drives the verdict paths by injecting synthetic peer rows through the worker's public API, then reads the `rabbitmq_aws_node_health_*` families through the real Prometheus text format.

Run them per `CONTRIBUTING.md`, for example `make eunit` for the unit and property suites, or `make ct-aws_node_health_SUITE` for the integration suite.

## In-vivo validation

The automated tests feed the scorer synthetic windows. The in-vivo procedure validates the two assumptions those windows encode, on a real cluster: that a continuous packet-loss fault flaps the failure-detector probability at onset and then pins it high (rather than re-normalising downward), and that the paths fire as designed end to end. It also confirms that a degraded node stays attributed for the duration of the fault, with no healthy node ever misattributed.

### Recovering the firing path

`aws_node_health:analyze/2` returns only the verdict (`clean` | `{suspect, Node}` | `cluster_wide`); it does not report which of P1, P2, or P3 attributed a suspect, and the published `suspected` and `confidence` gauges do not distinguish the paths at steady state (P2 and P3 both approach a confidence of ~1.0). Timing inference is unreliable because each path's warmup depends on how full the window is.

The reliable method reads the path directly. The worker's `#state{}` holds the live window and the analysis config, reachable with `sys:get_state/1`. The probe below fetches both, calls the real `analyze/2` for the ground-truth verdict, and recomputes the P1/P2/P3 fire predicates against the same window. It cross-checks its recomputed verdict against the real one and reports `match, ok` when they agree, so a faithful reproduction is proven rather than assumed, and the fire booleans can be trusted.

> [!NOTE]
> The probe reads `#state{}` fields positionally: the window is field 7 and the analysis config is field 11 as of this writing. Confirm these indices against `aws_node_health_worker.erl` if the record layout changes.

```erlang
begin
  S = sys:get_state(aws_node_health_worker),
  Window = element(7, S),
  Cfg = element(11, S),
  Official = aws_node_health:analyze(Cfg, Window),
  Nodes = lists:usort(lists:flatten([[Obs | maps:keys(V)] || Snap <- Window, {Obs, V} <- maps:to_list(Snap)])),
  Median = fun(L0) -> L = lists:sort(L0), Nn = length(L), case Nn rem 2 of 1 -> float(lists:nth(Nn div 2 + 1, L)); 0 -> (lists:nth(Nn div 2, L) + lists:nth(Nn div 2 + 1, L)) / 2 end end,
  SnapInbound = fun(Snap, X) -> Vals = [maps:get(X, V) || {Obs, V} <- maps:to_list(Snap), Obs =/= X, is_map(V), maps:is_key(X, V)], case Vals of [] -> none; _ -> {ok, Median(Vals)} end end,
  InSeries = fun(X) -> [M || Snap <- Window, {ok, M} <- [SnapInbound(Snap, X)]] end,
  MedOf = fun(X) -> case InSeries(X) of [] -> 0.0; Sr -> Median(Sr) end end,
  Extreme = maps:get(extreme, Cfg),
  Denom = max(length(Window), maps:get(window, Cfg, length(Window))),
  FracOf = fun(X) -> Sr = InSeries(X), length([1 || V <- Sr, V >= Extreme]) / max(1, Denom) end,
  Upcross = fun(Sr) -> element(1, lists:foldl(fun(V, {Acc, Prev}) -> case is_number(Prev) andalso Prev < Extreme andalso V >= Extreme of true -> {Acc + 1, V}; false -> {Acc, V} end end, {0, undefined}, lists:reverse(Sr))) end,
  FlapOf = fun(X) -> Upcross(InSeries(X)) end,
  OwnViewMed = fun(Obsvr, Peer) -> Vs = lists:filtermap(fun(Snap) -> case maps:get(Obsvr, Snap, undefined) of Vm when is_map(Vm) -> case maps:find(Peer, Vm) of {ok, P} -> {true, P}; error -> false end; _ -> false end end, Window), case Vs of [] -> none; _ -> {ok, Median(Vs)} end end,
  OwnOutMin = fun(X) -> PM = [M || Peer <- Nodes, Peer =/= X, {ok, M} <- [OwnViewMed(X, Peer)]], case PM of [] -> 0.0; _ -> lists:min(PM) end end,
  OwnSeen = fun(X) -> lists:any(fun(Peer) -> OwnViewMed(X, Peer) =/= none end, [P || P <- Nodes, P =/= X]) end,
  Med = maps:from_list([{N, MedOf(N)} || N <- Nodes]),
  Frac = maps:from_list([{N, FracOf(N)} || N <- Nodes]),
  Flaps = maps:from_list([{N, FlapOf(N)} || N <- Nodes]),
  OwnOut = maps:from_list([{N, OwnOutMin(N)} || N <- Nodes]),
  Sorted = lists:sort(Nodes),
  ArgMax = fun(ScoreMap) -> lists:foldl(fun(N, B) -> case maps:get(N, ScoreMap) > maps:get(B, ScoreMap) of true -> N; false -> B end end, hd(Sorted), tl(Sorted)) end,
  Cand = ArgMax(Med),
  Others = [N || N <- Nodes, N =/= Cand],
  OthersMaxMed = lists:max([0.0 | [maps:get(N, Med) || N <- Others]]),
  Marg = maps:get(Cand, Med) - OthersMaxMed,
  NumElev = length([N || N <- Nodes, maps:get(N, Med) >= maps:get(elevated, Cfg)]),
  P2Fire = Marg >= maps:get(margin, Cfg) andalso maps:get(Cand, Frac) >= maps:get(extreme_frac, Cfg),
  CW = NumElev >= maps:get(cluster_min_nodes, Cfg),
  CandOwn = maps:get(Cand, OwnOut),
  OthersOwnMax = lists:max([0.0 | [maps:get(N, OwnOut) || N <- Others]]),
  RowsPresent = lists:all(fun(N) -> OwnSeen(N) end, Others),
  P3Fire = maps:get(Cand, Med) >= maps:get(bidir_inbound, Cfg) andalso CandOwn >= maps:get(bidir_outbound, Cfg) andalso CandOwn - OthersOwnMax >= maps:get(bidir_margin, Cfg) andalso RowsPresent,
  P1Cand = ArgMax(Flaps),
  P1Others = [N || N <- Nodes, N =/= P1Cand],
  Pristine = maps:get(pristine, Cfg),
  FlapMin = maps:get(flap_min, Cfg),
  P1OthersPristine = lists:all(fun(N) -> maps:get(N, Med) < Pristine andalso maps:get(N, Flaps) < FlapMin end, P1Others),
  P1Fire = P1OthersPristine andalso maps:get(P1Cand, Flaps) >= FlapMin,
  MyVerdict = if P2Fire -> {suspect, Cand, p2}; P3Fire -> {suspect, Cand, p3}; CW -> cluster_wide; P1Fire -> {suspect, P1Cand, p1}; true -> clean end,
  OffV = maps:get(verdict, Official),
  Match = case {OffV, MyVerdict} of {clean, clean} -> ok; {cluster_wide, cluster_wide} -> ok; {{suspect, X}, {suspect, X, _}} -> ok; _ -> mismatch end,
  CS = InSeries(Cand),
  NHi = length([1 || V <- CS, V >= 0.9]),
  NMid = length([1 || V <- CS, V >= 0.5, V < 0.9]),
  NLo = length([1 || V <- CS, V < 0.5]),
  {off, OffV, my, MyVerdict, cand, Cand, cmed, maps:get(Cand, Med), cfrac, maps:get(Cand, Frac), cflap, maps:get(Cand, Flaps), cown, maps:get(Cand, OwnOut), buckets, {NHi, NMid, NLo, length(CS)}, fires, [P2Fire, P3Fire, CW, P1Fire], match, Match}
end.
```

Save the expression to a file on the observed node, for example `/tmp/nh_probe.erl`, and run it with `rabbitmqctl eval "$(cat /tmp/nh_probe.erl)"`. The result fields are: `off` the real verdict, `my` the recomputed verdict tagged with the firing path, `cand` the candidate, `cmed`/`cfrac`/`cflap`/`cown` the candidate's inbound median, extreme fraction, flap count, and own-outbound minimum, `buckets` the candidate's inbound-series distribution as `{>=0.9, 0.5..0.9, <0.5, total}`, `fires` the `[P2, P3, cluster_wide, P1]` predicate booleans, and `match` the cross-check (`ok` or `mismatch`).

### Fault injection

Degrade one node with `tc netem` on its cluster interface. A single-interface egress-loss qdisc is unidirectional at the failure detector (the degraded node still receives its peers' heartbeats), which is why P3 does not fire for it; a genuinely bidirectional fault would require loss in both directions.

```
# Apply, on the node to degrade (interface is cluster-specific, e.g. ens5):
sudo tc qdisc add dev ens5 root netem loss 50%

# Remove:
sudo tc qdisc del dev ens5 root
```

Always pair the fault with a self-expiry timer so a dropped session cannot leave a node degraded, and confirm the qdisc is gone afterwards.

### Driver

This driver injects loss on one node, polls a healthy node's probe once per interval (printing elapsed time, whether the fault is still applied, and the recovered path), then cleans up. It takes SSH targets, the interface, and the `rabbitmqctl` path as parameters, and assumes the probe expression above is present on the observed node. Supply your own SSH access to the cluster.

```bash
#!/usr/bin/env bash

# In-vivo node-health validation run against a live cluster. Injects packet
# loss on one node, polls a healthy node's probe once per interval (printing
# elapsed time, whether the fault is still applied, and the recovered path),
# then removes the fault. Assumes the probe expression above is saved on the
# observed node, and that you have SSH access to both nodes.

set -o errexit
set -o nounset
set -o pipefail

fault_ssh=${FAULT_SSH:?ssh target of the node to degrade}
poll_ssh=${POLL_SSH:?ssh target of a healthy node to observe}
iface=${IFACE:-ens5}
rabbitmqctl=${RABBITMQCTL:-rabbitmqctl}   # path to rabbitmqctl on poll_ssh
probe=${PROBE:-/tmp/nh_probe.erl}         # path to the probe on poll_ssh
loss=${LOSS:-50%}
duration=${DURATION:-135}                 # seconds the fault stays applied
interval=${INTERVAL:-3}                   # seconds between polls

# The self-expiry timer outlives the run so a dropped session cannot leave a
# node degraded.
ssh "$fault_ssh" "sudo tc qdisc add dev $iface root netem loss $loss; \
  nohup sh -c 'sleep $((duration + 30)); sudo tc qdisc del dev $iface root' >/dev/null 2>&1 &" < /dev/null

onset=$(date +%s)
end=$((onset + duration))

# Each line correlates the fault's presence with the recovered verdict and path.
while (( $(date +%s) < end ))
do
    elapsed=$(( $(date +%s) - onset ))
    netem=$(ssh "$fault_ssh" "sudo tc qdisc show dev $iface | grep -oE 'loss [0-9.]+%' || echo GONE" < /dev/null)
    verdict=$(ssh "$poll_ssh" "$rabbitmqctl eval \"\$(cat $probe)\"" < /dev/null | tr -d '\n' | tr -s ' ')
    echo "t=${elapsed}s netem=[$netem] $verdict"
    sleep "$interval"
done

ssh "$fault_ssh" "sudo tc qdisc del dev $iface root 2>/dev/null; sudo tc qdisc show dev $iface | head -1" < /dev/null
```

### Representative results

A continuous 50% egress-loss fault on a three-node cluster, observed with the driver above (`match, ok` on every sample, so the recovered path is trustworthy):

- Onset: the candidate is attributed within a few seconds via P1. Stochastic loss oscillates the probability across the extreme threshold before it pins high, and those crossings trip the flap path well before the extreme fraction can fill P2's gate.
- Sustained: by roughly `extreme_frac * window` ticks (about 24s at the defaults) the extreme fraction fills, the probability is pinned (inbound median 1.0, all window samples extreme), the flap count decays to zero so P1 no longer fires, and P2 holds the suspect for the remainder of the fault. Verified with the fault confirmed present on every poll, the verdict never fell back to `clean`.
- P3 never fired for this egress-only fault: the degraded node still saw its peers normally, so its own-outbound minimum stayed near zero.
- No healthy node was attributed at any point, and the verdict returned to `clean` after the fault was removed and the window flushed.

The onset-via-P1 and sustained-via-P2 behaviours are pinned as regression guards in `test/aws_node_health_tests.erl` (`continuous_loss_onset_is_attributed_via_p1_test` and `sustained_egress_fault_is_held_via_p2_without_flaps_test`).

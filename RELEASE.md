# `amazon-mq/rabbitmq-aws` release process

Here are the commands to run when releasing a new version of this project:

```
readonly VER=0.1.0
git checkout -b "rabbitmq-aws-$VER"
sed -i.bak "s/^PROJECT_VERSION =.*/PROJECT_VERSION = $VER/" Makefile
github_changelog_generator --future-release "$VER" --user amazon-mq --project rabbitmq-aws --token "$GITHUB_API_TOKEN"

# Optional - remove last line of CHANGELOG.md

git add CHANGELOG.md
git commit -a -m "rabbitmq-aws $VER"
git push -u origin "rabbitmq-aws-$VER"
gh pr create --fill
```

* Ensure CI runs successfully for the PR
* Merge PR

```
git checkout main
git pull origin main
git remote prune origin
git branch -d "rabbitmq-aws-$VER"
git tag --annotate --sign --local-user=GPGKEYID --message="rabbitmq-aws $VER" "$VER"
git push --tags
```

* Ensure that Erlang/OTP 26.x and Elixir 1.16.x-otp-26 are in your `PATH`

```
git clone --branch v4.2.0 https://github.com/rabbitmq/rabbitmq-server.git rabbitmq-server_4.2.0
cd rabbitmq-server_4.2.0
git clone --branch "$VER" git@github.com:amazon-mq/rabbitmq-aws.git deps/aws
make
make -C deps/aws
make -C DIST_AS_EZS deps/aws dist
gh release create "$VER" --verify-tag --title "$VER" --latest --generate-notes path/to/*.ez
```

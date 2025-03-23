task_manager
=====

An OTP application that runs simple tasks at a specific time.
Send report to Slack or console.

Application reads default config (`priv/default.config`).
And then try to read prod config, path get from `PROD_CONF` os env.
Prod config overwrites default config.

## Operations

- Build: `$ rebar3 compile`
- Release: `$ rebar3 as prod release`
- Connect to running node: `$ ./bin/task_manager remote_console`
- Send tick: `> tls_check_task ! tick.`

## Tasks

 - `tls_check_task` - sends a notification when a TLS certificate has expired or is about to expire (in 4 days)

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
- Do the job immediately: `> tls_check_task ! now.`
- Dialyzer, build PLT: `$ dialyzer --build_plt --apps erts kernel stdlib crypto sasl inets ssl public_key`
- Dialyzer, run: `$ dialyzer -r apps/task_manager/ --src`

## Tasks

 - `tls_check_task` - sends a notification when a TLS certificate has expired or is about to expire (in 4 days)

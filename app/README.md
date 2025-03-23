task_manager
=====

An OTP application that runs simple tasks at a specific time.
Send report to Slack or console.

Build
-----

    $ rebar3 compile

Release
-----
    $ rebar3 as prod release

Connect to running node
-----
    $ ./bin/task_manager remote_console
FROM erlang:27-slim AS builder

ARG REBAR_VERSION="3.24.0"
ARG REBAR_SOURCE="https://github.com/erlang/rebar3/releases/download/${REBAR_VERSION}/rebar3"

# Install needed packages and setup non-root user. Use a separate RUN statement to add your own dependencies.
RUN apt-get update \
  && export DEBIAN_FRONTEND=noninteractive \
  && apt-get -y install --no-install-recommends curl ca-certificates tzdata 2>&1 \
  #
  # Install Rebar3
  && curl -sSL ${REBAR_SOURCE} -o /usr/local/bin/rebar3 \
  && chmod a+x /usr/local/bin/rebar3 \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

  COPY app /app
  RUN cd /app \
  && rebar3 as prod release

FROM erlang:27-slim

RUN apt-get update \
  && export DEBIAN_FRONTEND=noninteractive \
  && apt-get -y install --no-install-recommends curl ca-certificates tzdata 2>&1 \
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/_build/prod/rel/task_manager /app
WORKDIR /app
CMD ["./bin/task_manager", "foreground"]

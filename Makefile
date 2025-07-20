# Makefile for taskerl application

REBAR3 = $(shell which rebar3)
APP_NAME = taskerl

.PHONY: init run stop test clean

init:
	@$(REBAR3) release

run:
	@_build/default/rel/$(APP_NAME)/bin/$(APP_NAME) daemon

stop:
	@_build/default/rel/$(APP_NAME)/bin/$(APP_NAME) stop

test:
	@$(REBAR3) test

clean:
	@-$(MAKE) stop >/dev/null 2>&1 || true
	@grep -v '^#' .gitignore | grep -v '^$$' | xargs -I{} rm -rf {}
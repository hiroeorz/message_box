ERLC=erlc
ERL=erl
ERLCFLAGS=-o
SRCDIR=src
LOGDIR=./log
BEAMDIR=./ebin ./deps/sqlite3/ebin
DBDIR=./db
APP_NAME=message_box
HOST_NAME=127.0.0.1
REBAR=./rebar
EDOWN=./make_doc

all: clean compile xref

compile:
	@$(REBAR) get-deps compile

xref:
	@$(REBAR) xref

clean: 
	@ $(REBAR) clean

check:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit 

edoc:
	@$(REBAR) doc

edown:
	@$(EDOWN)

cleardata:
	@ rm -rf $(DBDIR)

boot:
	@ $(ERL) -pa $(BEAMDIR) -name $(APP_NAME)@$(HOST_NAME) -s $(APP_NAME) start
ERLC=erlc
ERL=erl
ERLCFLAGS=-o
SRCDIR=src
LOGDIR=./log
BEAMDIR=./ebin
DBDIR=./db
APP_NAME=message_box

all: clean compile

compile:
	@./rebar compile

xref:
	@./rebar xref

clean: 
	@ ./rebar clean

eunit:
	@./rebar eunit

edoc:
	@./rebar doc

cleardata:
	@ rm -rf $(DBDIR)

boot:
	@ $(ERL) -pa $(BEAMDIR) -sname $(APP_NAME) -s $(APP_NAME) start
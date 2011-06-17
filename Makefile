ERLC=erlc
ERL=erl
ERLCFLAGS=-o
SRCDIR=src
LOGDIR=./log
BEAMDIR=./ebin
DBDIR=./db
APP_NAME=message_box

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ mkdir -p $(DBDIR) ;
	@ mkdir -p $(LOGDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
clean: 
	@ rm -rf $(BEAMDIR)/*.beam;
	@ rm -rf erl_crush.dump

cleardata:
	@ rm -rf $(DBDIR)

boot:
	@ $(ERL) -pa $(BEAMDIR) -sname $(APP_NAME) -s $(APP_NAME) start
ERLC=erlc
ERL=erl
ERLCFLAGS=-o
SRCDIR=src
LOGDIR=./log
BEAMDIR=./ebin

all: 
	@ mkdir -p $(BEAMDIR) ;
	@ $(ERLC) $(ERLCFLAGS) $(BEAMDIR) $(SRCDIR)/*.erl ;
	@ mkdir -p $(LOGDIR) ;
clean: 
	@ rm -rf $(BEAMDIR)/*.beam;
	@ rm -rf erl_crush.dump

boot:
	@ $(ERL) -pa ./ebin -sname message_box -s message_box start
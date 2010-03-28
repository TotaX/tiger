# Unix makefile for tigermain example

HOME_=${HOME}
MOSMLHOME=${HOME_}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac -v

GCC=gcc
CFLAGS= -g
MOSMLC=${MOSMLHOME}/bin/mosmlc -c -liberal
MOSMLL=${MOSMLHOME}/bin/mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=
DATE=`date +%d-%m-%y`
HOUR=`date +%H%M%S`
BACKUP_FILE=tiger.bkp.$(DATE)-$(HOUR).RELEASE.tar.bz2
BACKUP_DIR=$(HOME)/tiger/backup
# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

GRALOBJS= tigerabs.uo tigergrm.uo tigerlex.uo tigermain.uo \
	tigernlin.uo tigerpp.uo \
	tigertab.uo \
	tigertree.uo \
	tigerframe.uo \
	tigerstack.uo \
	tigertemp.uo \
	tigerescap.uo \
	tigertypes.uo \
	tigerenv.uo \
	tigerutils.uo \
	topsort.uo \
	tigerseman.uo \
	tigertranslate.uo 

all: tiger

tiger: $(GRALOBJS) $(OBJSGEN)
	$(MOSMLL) -o tiger$(EXEFILE) tigermain.uo

tigergrm.sml tigergrm.sig: tigergrm.y 
	$(MOSMLYACC) tigergrm.y

tigerlex.sml: tigerlex.lex
	$(MOSMLLEX) tigerlex.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) tigergrm.output
	$(REMOVE) tigergrm.sig
	$(REMOVE) tigergrm.sml
	$(REMOVE) tigerlex.sml
	$(REMOVE) tigermain
	$(REMOVE) *.ui
	$(REMOVE) *.uo
	$(REMOVE) errlist
	$(REMOVE) *.o
package: clean
	tar -cjf $(BACKUP_FILE)  *
	mv $(BACKUP_FILE) $(BACKUP_DIR)

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: tigerabs.sml tigergrm.sml tigerlex.sml tigermain.sml \
	tigernlin.sml tigerpp.sml \
       	tigertab.sml \
	tigerescap.sml \
	tigertypes.sml \
	tigerenv.sml \
	tigerutils.sml \
	topsort.sml \
	tigerseman.sml \
	tigerstack.sml \
	tigertemp.sml \
       	tigerframe.sml \
	tigertree.sml \
	tigertranslate.sml 
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
tigertranslate.uo: tigertranslate.ui tigertree.ui tigerframe.ui \
    tigerstack.ui 
tigertemp.uo: tigertemp.ui 
tigerutils.uo: tigertypes.uo tigertranslate.ui 
tigerseman.uo: tigerseman.ui tigerenv.uo tigertab.ui tigertypes.uo \
    topsort.uo tigertranslate.ui tigerstack.ui tigerabs.uo tigerutils.uo 
tigertree.ui: tigertemp.ui 
tigerframe.uo: tigerframe.ui tigertree.ui tigertab.ui tigerstack.ui \
    tigertemp.ui 
tigertranslate.ui: tigertree.ui tigerframe.ui tigerstack.ui tigertemp.ui \
    tigerabs.uo 
tigerescap.ui: tigerabs.uo 
tigerenv.uo: tigertypes.uo tigertranslate.ui 
tigerpp.uo: tigerabs.uo 
tigertab.uo: tigertab.ui 
tigerseman.ui: tigertypes.uo tigertranslate.ui tigerabs.uo 
tigercanon.ui: tigertree.ui 
tigerescap.uo: tigerescap.ui tigertab.ui tigerabs.uo tigerpp.uo 
tigerstack.uo: tigerstack.ui 
tigergrm.ui: tigerabs.uo 
tigerlex.uo: tigergrm.ui tigernlin.uo 
tigerframe.ui: tigertree.ui tigertab.ui tigerstack.ui tigertemp.ui 
tigermain.uo: tigerseman.ui tigerescap.ui tigergrm.ui tigertranslate.ui \
    tigerlex.uo tigerpp.uo 
tigergrm.uo: tigergrm.ui tigernlin.uo tigerabs.uo 
tigertree.uo: tigertree.ui tigertemp.ui 
tigercanon.uo: tigercanon.ui tigertree.ui tigertab.ui tigertemp.ui 

SRC_DIR = ./src
BIN_DIR = ./bin

####################################################################
#
# set "GS_AIO=-DUSE_AIO" to use async READ from tape
#
#####################################################################

linux: OPT =  "GS_ONLINE=" "GS_AIO=-DUSE_AIO" "GS_OPT=-O2" linux
linux: MAKE = gmake
linux: Libr Cmat Gsort Recal Sadd Stopp Tape Xtrack

intel: OPT =  "GS_ONLINE=" "GS_AIO=-DUSE_AIO" "GS_OPT=-O2" intel
intel: MAKE = make
intel: Libr Cmat Gsort Recal Sadd Stopp Tape Xtrack

sun: OPT = "GS_ONLINE=" "GS_AIO=-DUSE_AIO" sun
sun: MAKE = gmake
sun: Libr Cmat Gsort Recal Sadd Stopp Tape Xtrack

digital:= OPT = "GS_ONLINE=" "GS_AIO=-DUSE_AIO" digital
digital:= MAKE = make
digital: Libr Cmat Gsort Recal Sadd Stopp Tape Xtrack




Libr:
	@cd $(SRC_DIR)/libr && $(MAKE) $(OPT)
	@cd $(SRC_DIR)/Ygl-4.0 && $(MAKE) $(OPT)

Cmat:
	@cd $(SRC_DIR)/cmat && $(MAKE) $(OPT)
	mv $(SRC_DIR)/cmat/cmat $(BIN_DIR)

Gsort:
	@cd $(SRC_DIR)/gsort && $(MAKE) $(OPT)
	mv $(SRC_DIR)/gsort/gsort $(BIN_DIR)
	mv $(SRC_DIR)/gsort/mat_stop $(BIN_DIR)
Recal:
	@cd $(SRC_DIR)/recal && $(MAKE) $(OPT)
	mv $(SRC_DIR)/recal/recal_cob $(BIN_DIR)
	mv $(SRC_DIR)/recal/recal_corr $(BIN_DIR)
	mv $(SRC_DIR)/recal/recal_diff $(BIN_DIR)
	mv $(SRC_DIR)/recal/recal_doppl $(BIN_DIR)
	mv $(SRC_DIR)/recal/recal_gain $(BIN_DIR)
	mv $(SRC_DIR)/recal/recal_test $(BIN_DIR)
	mv $(SRC_DIR)/recal/recal_time $(BIN_DIR)

Sadd:
	@cd $(SRC_DIR)/sadd && $(MAKE) $(OPT)
	mv $(SRC_DIR)/sadd/sadd $(BIN_DIR)

Stopp:
	@cd $(SRC_DIR)/StopP && $(MAKE) $(OPT)
	mv $(SRC_DIR)/StopP/stopp $(BIN_DIR)

Tape:
	@cd $(SRC_DIR)/tape && $(MAKE) $(OPT)
	mv $(SRC_DIR)/tape/list_tape $(BIN_DIR)
	mv $(SRC_DIR)/tape/tapetotape $(BIN_DIR)

Xtrack:
	@cd $(SRC_DIR)/xtrack && $(MAKE) $(OPT)
	mv $(SRC_DIR)/xtrack/xtrackn $(BIN_DIR)

LasIn:
	cp $(SRC_DIR)/libr/laslib.init $(HOME)/.laslib.init
	
clean:
	@cd $(SRC_DIR)/Ygl-4.0 && $(MAKE) clean
	@cd $(SRC_DIR)/cmat && $(MAKE) clean
	@cd $(SRC_DIR)/gsort && $(MAKE) clean
	@cd $(SRC_DIR)/libr && $(MAKE) clean
	@cd $(SRC_DIR)/recal && $(MAKE) clean
	@cd $(SRC_DIR)/sadd && $(MAKE) clean
	@cd $(SRC_DIR)/StopP && $(MAKE) clean
	@cd $(SRC_DIR)/tape && $(MAKE) clean
	@cd $(SRC_DIR)/xtrack && $(MAKE) clean



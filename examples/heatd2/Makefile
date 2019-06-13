MPICC    ?= mpicc
MCC = mcc

TCLFLAGS= --checkpoint
#DEBUG_FLAGS=-g -O0 -k --verbose
FLAGS=$(DEBUG_FLAGS)

EXECUTABLE=heatd2

FTI_CKPT_DIR=/p/home/jusers/maronas1/deep/openchk-examples/fti-local-dir
FTI_GLOBAL_DIR=/p/home/jusers/maronas1/deep/openchk-examples/fti-global-dir
FTI_META_DIR=/p/home/jusers/maronas1/deep/openchk-examples/.fti

all: $(EXECUTABLE)

$(EXECUTABLE): heatd2.c
	$(MPICC) -cc=$(MCC) -o $@ $< $(LDFLAGS) $(CFLAGS) $(MCCFLAGS) $(TCLFLAGS)
clean:
	rm -rf $(EXECUTABLE) 
clean-fti:
	rm -rf $(FTI_CKPT_DIR)/* $(FTI_GLOBAL_DIR)/* $(FTI_META_DIR)/*

MPIFC    ?= mpif90
MFC = ifort-mfc

TCLFLAGS= --checkpoint
#DEBUG_FLAGS=-g -O0 -k --verbose
OMP = -qopenmp 
CFLAGS = -O3 -march=native $(DEBUG_FLAGS) $(OMP)
LDFLAGS= -lm

EXECUTABLE=stream

FTI_CKPT_DIR=/p/home/jusers/maronas1/deep/openchk-examples/fti-local-dir
FTI_GLOBAL_DIR=/p/home/jusers/maronas1/deep/openchk-examples/fti-global-dir
FTI_META_DIR=/p/home/jusers/maronas1/deep/openchk-examples/.fti


all: \
$(EXECUTABLE) 

$(EXECUTABLE): stream.f90
	$(MPIFC) -fc=$(MFC) $(DEBUG_FLAGS) $(TCLFLAGS) $(CFLAGS) -o $@ $^ $(LDFLAGS) 

clean:
	rm $(EXECUTABLE)
clean-fti:
	rm -r $(FTI_CKPT_DIR)/* $(FTI_GLOBAL_DIR)/* $(FTI_META_DIR)/*

#include ../Makefile.inc

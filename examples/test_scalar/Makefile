MPICXX    ?= mpicxx
MPIF90    ?= mpif90
MCXX = mcxx
MFC = ifort-mfc

TCLFLAGS= --checkpoint
#DEBUG_FLAGS=-g -O0 -k --verbose
FLAGS=$(DEBUG_FLAGS)

EXECUTABLE=test_scalar

FTI_CKPT_DIR=/p/home/jusers/maronas1/deep/openchk-examples/fti-local-dir
FTI_GLOBAL_DIR=/p/home/jusers/maronas1/deep/openchk-examples/fti-global-dir
FTI_META_DIR=/p/home/jusers/maronas1/deep/openchk-examples/.fti

all: \
$(EXECUTABLE)-fortran $(EXECUTABLE)-cpp 

$(EXECUTABLE)-cpp: test_scalar.cpp
	$(MPICXX) -cxx=$(MCXX) $(DEBUG_FLAGS) $(TCLFLAGS) -o $@ $^
$(EXECUTABLE)-fortran: test_scalar.f90
	$(MPIF90) -fc=$(MFC) $(DEBUG_FLAGS) $(TCLFLAGS) -o $@ $^

clean:
	rm $(EXECUTABLE)-fortran $(EXECUTABLE)-cpp
clean-fti:
	rm -r $(FTI_CKPT_DIR)/* $(FTI_GLOBAL_DIR)/* $(FTI_META_DIR)/*

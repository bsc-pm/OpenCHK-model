# OpenCHK model

The **OpenCHK** model is a pragma-based checkpointing model developed by the [*Programming Models group*](https://pm.bsc.es/)
at the [**Barcelona Supercomputing Center**](http://www.bsc.es/).

The aim of this model is to provide a generic and portable way to checkpoint and recover data in C/C++ and Fortran High Performance Computing applications.

The prototype implementation of the model is based on two software components:
- [*Mercurium source-to-source compiler*](https://github.com/bsc-pm/mcxx)
- [*Transparent Checkpoint Library*](https://github.com/bsc-pm/TCL)


## The model
The **OpenCHK** model is a pragma-based checkpointing model. The sentinel used to recognized these pragmas is `chk`.
 
### Initialization and Shutdown

    #pragma chk init [clauses]
    
    
    #pragma chk shutdown [clauses]
    
### Checkpointing and recovering some data

    #pragma chk load
    #pragma chk store

 
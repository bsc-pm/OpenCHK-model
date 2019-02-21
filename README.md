# OpenCHK model

The **OpenCHK** model is a pragma-based checkpointing model developed by the [*Programming Models group*](https://pm.bsc.es/)
at the [**Barcelona Supercomputing Center**](http://www.bsc.es/).

The aim of this model is to provide a generic and portable way to checkpoint and recover data in C/C++ and Fortran High Performance Computing applications.

The prototype implementation of the model is based on two software components:
- [*Mercurium source-to-source compiler*](https://github.com/bsc-pm/mcxx)
- [*Transparent Checkpoint Library*](https://github.com/bsc-pm/TCL)


## The model
The **OpenCHK** model is a pragma-based checkpointing model. The sentinel used to recognized this kind of pragmas is `chk`.

 
### Initialization
##### Description
The *init* construct defines the initialization of a checkpoint context.

    #pragma chk init [clauses]  // C/C++ syntax
    !$chk init       [clauses]  // Fortran syntax
    
where clauses may be:
    
    com(comm-expr)

The *comm* clause is used to specify the MPI communicator that should be used in the checkpoint context that is being created.

##### Restrictions
- At most one checkpoint context may be alive at the same time.
- It is mandatory to specify a MPI communicator through the *comm* clause.

### Shutdown
##### Description
The *shutdown* construct defines the finalization of a checkpoint context.

    #pragma chk shutdown  // C/C++ syntax 
    !$chk shutdown        // Fortran syntax
    

### Recovering some data
##### Description
The *load* construct specifies that some variables and memory regions are going to be updated with the stored values that we have for them in a previous checkpoint (if any).

    #pragma chk load(data-expr-list)  [clauses] // C/C++ syntax
    !$chk load(data-expr-list)        [clauses] // Fortran syntax
    
where clauses may be:

    if(bool-expr)
    
The *if* clause is used as a switch off mechanism: the contruct will be ignored if the expression of the *if* clause evaluates to false.
    
#### Restrictions
- It must be used inside a checkpoint context.

### Checkpointing some data
##### Description
The *store* construct specifies that some variables and memory regions are going to be saved in a new checkpoint.

    #pragma chk store(data-expr-list)  [clauses] // C/C++ syntax
    !$chk store(data-expr-list)        [clauses] // Fortran syntax
    
where clauses may be:

    if(bool-expr)
    id(integer-expr)
    level(integer-expr)
    kind(kind-expr)
    
The *if* clause is used as a switch off mechanism: the contruct will be ignored if the expression of the *if* clause evaluates to false.

The *id* clause is used to give a name to the new checkpoint.

The *level* clause is used to specify in which level the checkpoint should be done.

The *kind* clause is used to specify which kind of checkpoint you want to do. We support two different kinds of checkpoint right now: `CHK_FULL` and `CHK_DIFF`. If the *kind* clause is not present, the default kind will be `CHK_FULL`.
    
#### Restrictions
- It must be used inside a checkpoint context.
- It is mandatory to specify an identifier through the *id* clause.
- It is mandatory to specify the checkpoint level through the *level* clause.
 
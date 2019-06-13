#include <iostream>
#include <tcl.h>
#include <cassert>
#include <stdlib.h>

void error_handler(int err_code) 
{
    std::cout << "Error inside CheckpointLib. Error code: " << err_code << "." << std::endl;
    exit(-1);
}

int main(int argc, char **argv) 
{
    int err = MPI_Init(&argc, &argv);
    assert(err == MPI_SUCCESS);
    MPI_Comm comm = MPI_COMM_WORLD;

    int inject_error = -1;
    if(argc == 2) {
        inject_error = std::atoi(argv[1]);
	std::cout << "Inject error at step " << inject_error << "." << std::endl;
    }

    #pragma chk init comm(comm)
    {
        int data, i = 0;
        bool restored = false;

        #pragma chk load(i, data)
        if(i != 0) {
            std::cout << "Restored data from iteration " << i << ". data = " << data << "." << std::endl;
            restored = true;
        }
        for(i; i < 10; i++) {
            data = i;
            #pragma chk store(i, data) kind(CHK_FULL) id(i) level((i%4)+1) if(1) handler(error_handler)
            if(i == inject_error && !restored) {
                std::cout << "Injected error." << std::endl;
                exit(-1);
            }
            std::cout << "Completed step " << i << std::endl;
        }
    }
    #pragma chk shutdown

    MPI_Finalize();
}

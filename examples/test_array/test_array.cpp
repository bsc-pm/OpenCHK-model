#include <iostream>
#include <cstdlib>
#include <tcl.h>
#include <cassert>

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

    int i = 0;
    int data[1024*1024];
    bool restored = false;

    #pragma chk init comm(comm)
    {

        #pragma chk load(i, data)
        if(i != 0) {
            std::cout << "Restored data from iteration " << i << ". data[i] = " << data[i] << "." << std::endl;
            restored = true;
        }
        for(i; i < 1024*1024; i++) {
            data[i] = i;
            #pragma chk store(i, data) kind(CHK_FULL) id(i) level((i%4)+1) if((i%(1024*128))==1) handler(error_handler)
            if(i == inject_error && !restored) {
                std::cout << "Injected error." << std::endl;
                exit(-1);
            }
        }
    }
    #pragma chk shutdown

    for(i = 0; i < 1024*1024; i++) 
        assert(i == data[i]);

    MPI_Finalize();
}

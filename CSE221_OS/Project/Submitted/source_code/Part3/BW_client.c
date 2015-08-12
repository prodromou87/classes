#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>  
#include <sys/socket.h>
#include <netinet/in.h>
#include <pthread.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <netdb.h>
#include <sys/time.h>
#include <arpa/inet.h>
#include <math.h>

typedef unsigned long long ticks;

ticks getticks(void)
{	
     unsigned a, d;
     //asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

int
main(int argc, char *argv[]){
	int listen_port;
	int msg_size = 1000000;
	int tries;
	struct sockaddr_in addr_serv;
	struct sockaddr_in addr_client;
	int sckTemp;
	int sckListen;
	socklen_t sinSize;
	int i, j, k;
	int tmp;
	char input[msg_size];
  	struct sockaddr_in switchAddr;
	int skt;
	char msg[msg_size];

	if (argc!=4){  	
		printf("Usage: ./BW_client <number> <IPAdress> <samples>\n");
		exit(1);
	}
  
	listen_port = atoi(argv[1]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Invalid Port\n");
		exit(1);
	}
  
	tries = atoi(argv[3]);
	if(tries < 1){
		printf("\nInvalid samples number (< 1)\n");
		exit(1);
	}	

	ticks results[tries];  
	ticks start, end;  
	  
	//Initialize
	for(i=0; i<msg_size; i++){
		msg[i] = 'a';
	}
		
	//Client: set server address
	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); //set port num
	switchAddr.sin_addr.s_addr = inet_addr(argv[2]);
  
	//Client: connect
	skt = socket(AF_INET,SOCK_STREAM,0);//Create socket

	connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr))
		

  	for(msg_size = 4096; msg_size<=0.5*1024*1024; msg_size+=16384){
	
	    for(i=0; i<tries; i++){	
		    //Client: Send message
		    if(write(skt, msg, msg_size) < 0){
			    perror("write socket");
		    } 
	    }
	
	}	
	
	if(write(skt, "END", strlen("END")) < 0){
		perror("write socket");
	} 

	close(skt); 

	return 0;
}

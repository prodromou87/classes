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
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

int
main(int argc, char *argv[]){
	int listen_port;
	int msg_size;
	int tries;
    int sckTemp;
	int sckListen;
	socklen_t sinSize;
	int i, j, k;
	int tmp;
	char input[1000];
  	struct sockaddr_in switchAddr;
	int skt;

	if (argc!=5){  	
		printf("Usage: ./RTT_client <port> <IPAdress> <msg_size> <samples>\n");
		exit(1);
	}
  
	listen_port = atoi(argv[1]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nInvalid Port\n");
		exit(1);
	}
  
	msg_size = atoi(argv[3]);
	if(msg_size < 1){
		printf("\nmsg_size\n");
		exit(1);
	}	

	tries = atoi(argv[4]);
	if(tries < 1){
		printf("\ntries\n");
		exit(1);
	}	

	
	char msg[msg_size];
	ticks results[tries];  
	ticks start, end;  
	  
	for(i=0; i<msg_size; i++){
		msg[i] = 'a';
	}

	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); 
	switchAddr.sin_addr.s_addr = inet_addr(argv[2]);
  
	skt = socket(AF_INET,SOCK_STREAM,0);

	connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr))

  
	for(i=0; i<tries; i++){	
		start = getticks();
		
		if(write(skt, msg, strlen(msg)) < 0){
			perror("write socket");
		} 

		bzero(input, sizeof (input)); 
		tmp = read(skt, input, sizeof (input));

		end = getticks();
		
		results[i] = end-start;
	}
	
	if(write(skt, "END", strlen("END")) < 0){
			perror("write socket");
	} 

	close(skt); 
	
	
	for(i=0; i<tries; i++){
		printf("RTT (cycles): %llu\n", results[i]);
		
	}

	
	return 0;
}

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

static __inline__ ticks getticks(void)
{	
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

int
main(int argc, char *argv[]){
	int listen_port;
	int msg_size = 1100000;
	int tries;
	struct sockaddr_in addr_serv;
	struct sockaddr_in addr_client;
    int sckTemp;
	int sckListen;
	socklen_t sinSize;
	int i, j, k;
	int tmp;
	char input[msg_size+1];
	char input2[msg_size+1];
  	struct sockaddr_in switchAddr;
	int skt;
	char msg[msg_size];

	//Check input parameters
	if (argc!=3){  	
		printf("USAGE: ./BW_loopback <port> <samples num>\n");
		exit(1);
	}
  
	//Set input port and message size
	listen_port = atoi(argv[1]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Invalid port\n");
		exit(1);
	}
	
	tries = atoi(argv[2]);
	if(tries < 1){
		printf("\nInvalid samples number (< 1)\n");
		exit(1);
	}	

	for(i=0; i<msg_size; i++){
		msg[i] = 'a';
	}
	
	ticks results[tries];  
	ticks start, end;

	//Server: assign socket - Binding
	sinSize = sizeof(struct sockaddr_in);  
	sckListen = socket(AF_INET,SOCK_STREAM,0);
	addr_serv.sin_family = AF_INET;
	addr_serv.sin_port = htons(listen_port);
	addr_serv.sin_addr.s_addr = htonl(INADDR_ANY);
  
  
	if(bind(sckListen,(struct sockaddr *)&addr_serv,sizeof(struct sockaddr))<0){
		exit(1);
	}  

	//Server: listen for connections
	if(listen(sckListen,50)<0){
		perror("listen");
		exit(1);
	}

	//Client: set the server address
	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); //Set port number
	switchAddr.sin_addr.s_addr = inet_addr("127.0.0.1");


	//Client: connect
	skt = socket(AF_INET,SOCK_STREAM,0);
	int status_flags = fcntl(skt, F_GETFL, 0);
	fcntl(skt, F_SETFL, O_NONBLOCK); //Nonblocking connect

	connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr))
 
    //Server: wait for message - Blocking
    sckTemp = accept(sckListen,(struct sockaddr *)&addr_client, &sinSize);

	fcntl(skt, F_SETFL, status_flags);

	for(msg_size = 4096; msg_size<=0.5*1024*1024; msg_size+=2048){
	  for(i=0; i<tries; i++){ 
	  //Client: Send Message
		tmp=0;
		
			if((tmp = write(skt, msg, msg_size)) < 0){
				perror("write socket");
			}
		
		//Server: Initialize buffer and read
		bzero(input, msg_size); 
	
		tmp=0;
		
		start = getticks();
		tmp =read(sckTemp, input, msg_size);
		end = getticks();
		
		results[i] = end-start;
	}  
	
	double bw =0, avg;
	ticks sum =0;

	for(i=0; i<tries; i++){
	    sum += results[i];
	}
	avg = sum / (double)tries;
	 	
	bw = (msg_size / (double)(1024*1024)) / (double)(avg / (double)(3500000000)); //=> bytes / sec
	printf("\nSize: %d KB Peak BW: %lf MB/s\n", msg_size/1024, bw);
	}
	
	close(skt); 
	close(sckTemp);
	return 0;
}

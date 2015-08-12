//------------------------------------------------------------------------------------------------
// Loopback - Local TCP benchamrking
//------------------------------------------------------------------------------------------------

//------------------------------------------------------------------------------------------------
//Libraries used
//------------------------------------------------------------------------------------------------
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>  // For sockets
#include <sys/socket.h> // For sockets
#include <netinet/in.h> // For Internet sockets
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

//------------------------------------------------------------------------------------------------
//Constants used
//------------------------------------------------------------------------------------------------
#define BACKLOG 50
#define REP 100000
#define REP_PIPES 100
#define CTX_REP 100000

#define O_LOOP 10
#define O_READ 420
#define O_WRITE_PIPE 2141
#define O_READ_PIPE 2173

typedef unsigned long long ticks;

static __inline__ ticks getticks(void)
{	
     unsigned a, d;
     asm("cpuid");
     asm volatile("rdtsc" : "=a" (a), "=d" (d));

     return (((ticks)a) | (((ticks)d) << 32));
}

//------------------------------------------------------------------------------------------------
// Main function 
//------------------------------------------------------------------------------------------------
int
main(int argc, char *argv[]){
	//Variables
	//---------
	int listen_port;
	int msg_size;
	int tries;
	struct sockaddr_in serverAddr;
	struct sockaddr_in clientAddr;

	//Check input parameters
	//----------------------
	if (argc!=5){  	
		printf("\nERROR! Invalid number of parameters!\n");
		printf("Please use: ./server -port number -samples number_of_samples\n");
		exit(1);
	}
  
	//Set input port and message size
	//-------------------------------
	listen_port = atoi(argv[2]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Valid port range is 1000 to 32768!\n");
		exit(1);
	}
  
	msg_size = 1100000;
	
	tries = atoi(argv[4]);
	if(tries < 1){
		printf("\nERROR! Tries should be set to a positive value!\n");
		exit(1);
	}	

	//Variables
	//---------
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
	ticks results[tries];  
	ticks start, end;  
	  
	//Initialize message to be sent
	//-----------------------------
	for(i=0; i<msg_size; i++){
		msg[i] = 'a';
	}
	//msg[msg_size-1] = '\0';
	
	//Server: assign socket - Binding
	//-------------------------------
	sinSize = sizeof(struct sockaddr_in);  
	sckListen = socket(AF_INET,SOCK_STREAM,0);
	serverAddr.sin_family = AF_INET;
	serverAddr.sin_port = htons(listen_port);
	serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
  
	//printf(">> Server: Binding on Port: %d...", listen_port);
  
	if(bind(sckListen,(struct sockaddr *)&serverAddr,sizeof(struct sockaddr))<0){
		perror("bind");
		exit(1);
	}
	//else printf("done.\n");    

	//Server: listen for incomming connections
	//----------------------------------------
	//printf(">> Server: Listening for incoming connections...");
	if(listen(sckListen,BACKLOG)<0){
		perror("listen");
		exit(1);
	}
	//else printf("done.\n");

	
	//Client: set the server address
	//------------------------------
	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); //Set to the port number at which server is expecting to rcv
	switchAddr.sin_addr.s_addr = inet_addr("127.0.0.1");


	//Client: connect
	//---------------
	skt = socket(AF_INET,SOCK_STREAM,0);//Create socket
	int status_flags = fcntl(skt, F_GETFL, 0);
	fcntl(skt, F_SETFL, O_NONBLOCK); //Nonblocking connect

	if (connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr)) < 0){
		//perror("send_msg(): ERROR connecting");
	}
 
    //Server: wait for new message - Blocking
	//---------------------------------------
    sckTemp = accept(sckListen,(struct sockaddr *)&clientAddr, &sinSize);

	fcntl(skt, F_SETFL, status_flags);

	for(msg_size = 4096; msg_size<=0.5*1024*1024; msg_size+=2048){
	  for(i=0; i<tries; i++){ 
	  //Client: Send Message
		//--------------------
		tmp=0;
		
			if((tmp = write(skt, msg, msg_size)) < 0){
				perror("write socket");
			}
		
		//Server: Initialize buffer - read from socket
		//--------------------------------------------
		bzero(input, msg_size); 
	
		tmp=0;
		
		//RECORD START TIME
		start = getticks();
		tmp =read(sckTemp, input, msg_size);
		//RECORD END TIME
		end = getticks();
		
		results[i] = end-start;
	}  
	
	//Compute avg, stdv and time in ns
	//-------------------------------
	double bw =0, avg;
	ticks sum =0;

	for(i=0; i<tries; i++){
	    sum += results[i];
	}
	avg = sum / (double)tries;
	 	
	bw = (msg_size / (double)(1024*1024)) / (double)(avg / (double)(3500000000)); //=> bytes / sec
	printf("\nSize: %d KB Peak BW: %lf MB/s\n", msg_size/1024, bw);
	}
	
	//Close sockets
	//-------------
	close(skt); 
	close(sckTemp);
	return 0;
}

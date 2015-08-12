//------------------------------------------------------------------------------------------------
// Server application for remote TCP benchmarking
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

ticks getticks(void)
{	
     unsigned a, d;
     //asm("cpuid");
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
	int tries;
	int msg_size = 1000000;
	struct sockaddr_in serverAddr;
	struct sockaddr_in clientAddr;
 
	//Check input parameters
	//----------------------
	if (argc!=5){  	
		printf("\nERROR! Invalid number of parameters!\n");
		printf("Please use: ./server -port number -samples Num_of_Samples\n");
		exit(1);
	}
  
	//Set input port and message size
	//-------------------------------
	listen_port = atoi(argv[2]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Valid port range is 1000 to 32768!\n");
		exit(1);
	}
	
	//Set tries
	//---------
	tries = atoi(argv[4]); 
	if(tries<1){
		printf("\nERROR! Samples should be a positive number!\n");
		exit(1);
	}
  
	//Variables
	//---------
	int sckTemp;
	int sckListen;
	socklen_t sinSize;
	char input[msg_size];
	int i, j, k;
	int tmp;
  	struct sockaddr_in switchAddr;
	ticks results[tries];  
	ticks start, end;  
	
	//Server: assign socket - Binding
	//-------------------------------
	sinSize = sizeof(struct sockaddr_in);  
	sckListen = socket(AF_INET,SOCK_STREAM,0);
	serverAddr.sin_family = AF_INET;
	serverAddr.sin_port = htons(listen_port);
	serverAddr.sin_addr.s_addr = htonl(INADDR_ANY);
    
	if(bind(sckListen,(struct sockaddr *)&serverAddr,sizeof(struct sockaddr))<0){
		perror("bind");
		exit(1);
	}

	//Server: listen for incomming connections
	//----------------------------------------
	if(listen(sckListen,BACKLOG)<0){
		perror("listen");
		exit(1);
	}
	 
    //Server: wait for new message - Blocking
	//---------------------------------------
    sckTemp = accept(sckListen,(struct sockaddr *)&clientAddr, &sinSize);
     
	 
	 
	
	for(msg_size = 4096; msg_size<=0.5*1024*1024; msg_size+=16384){
	for (i=0; i<tries; i++) results[i]=0;

	for(i=0; i<tries; i++){ 
		//Server: Initialize buffer - read from socket
		//--------------------------------------------
		bzero(input, msg_size); 
	
		tmp=0;
		while(tmp<msg_size){
			//RECORD START TIME
			start = getticks();
			tmp+=read(sckTemp, input, msg_size-tmp);
			//RECORD END TIME
			end = getticks();
			
			results[i] += end-start;
		}
	}  
	
	//Compute avg, stdv and time in ns
	//-------------------------------
	double avg =0, bw=0;
	ticks sum =0;
	
	for(i=0; i<tries; i++){
	    sum += results[i];
	}	
	avg = sum / (double)(tries);
	bw = (msg_size / (double)(1024*1024)) / (double)(avg / (double)(3500000000)); //=> Mbytes / sec

	printf("Size: %d KB Peak BW: %lf MB/s\n", msg_size / 1024, bw);
	}
 
	 
	 
	 
/*	 
	while (1){
		//Server: Initialize buffer - read from and send to socket
		//--------------------------------------------------------
		bzero(input, sizeof (input)); 
		tmp = read(sckTemp, input, sizeof(input), 0);
		printf("\nRead %d\n", tmp/1024);
		if(!strcmp(input, "END")) break;
		//printf("\n>> Server: Received msg \"%s\"", input);
		//printf("\n>> Server: Sending msg \"%s\"\n", input);
		//if(write(sckTemp, input, strlen(input)) < 0){
		//	perror("write socket");
		//}
	}

	//Compute avg, stdv and time in ns
	//-------------------------------
	double avg =0, stdv=0, t;
	ticks sum =0;
	
	for(i=0; i<tries; i++){
		avg+= msg_size/((double)results[i]/2.4)/tries;
	}	

	for(i=0; i<tries; i++){
		stdv += (msg_size/((double)results[i]/2.4)-avg)*(msg_size/((double)results[i]/2.4)-avg);
		results[i]=0;
	}
	stdv = stdv/tries;
	stdv = sqrt(stdv);

	printf("\nSize: %d KB Peak BW: %lf MB/s\n Standart deviation: %lf\n", msg_size/1024, avg*1000000000/1024/1024, stdv*1000000000/1024/10024);
*/

	//Close socket
	//------------
	close(sckTemp);

	return 0;
}

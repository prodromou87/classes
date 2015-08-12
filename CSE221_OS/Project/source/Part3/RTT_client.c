//------------------------------------------------------------------------------------------------
// Client applications for remote TCP benchamrking
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
	int msg_size;
	int tries;
	struct sockaddr_in serverAddr;
	struct sockaddr_in clientAddr;

	//Check input parameters
	//----------------------
	if (argc!=9){  	
		printf("\nERROR! Invalid number of parameters!\n");
		printf("Please use: ./server -port number -ip IPAdress -msg_size size -samples number\n");
		exit(1);
	}
  
	//Set input port and message size
	//-------------------------------
	listen_port = atoi(argv[2]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Valid port range is 1000 to 32768!\n");
		exit(1);
	}
  
	msg_size = atoi(argv[6]);
	if(msg_size < 1){
		printf("\nERROR! Msg size should be set to a positive value!\n");
		exit(1);
	}	

	tries = atoi(argv[8]);
	if(tries < 1){
		printf("\nERROR! Msg size should be set to a positive value!\n");
		exit(1);
	}	

	//Variables
	//---------
	int sckTemp;
	int sckListen;
	socklen_t sinSize;
	int i, j, k;
	int tmp;
	char input[1000];
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
		
	//Client: set the server address
	//------------------------------
	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); //Set to the port number at which server is expecting to rcv
	switchAddr.sin_addr.s_addr = inet_addr(argv[4]);
  
	//Client: connect
	//---------------
	skt = socket(AF_INET,SOCK_STREAM,0);//Create socket
	//fcntl(skt, F_SETFL, O_NONBLOCK); //Nonblocking connect

	if (connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr)) < 0){
		//perror("send_msg(): ERROR connecting");
	}

	//printf(">> Client: Connecting...");
  
	for(i=0; i<tries; i++){	
		
		//RECORD START TIME
		start = getticks();
		
		//Client: Send and receive message
		//--------------------------------
		//printf("\n>> Client: Sending msg \"%s\"", msg);
		if(write(skt, msg, strlen(msg)) < 0){
			perror("write socket");
		} 

		bzero(input, sizeof (input)); 
		tmp = read(skt, input, sizeof (input));
		//printf("\n>> Client: Received msg \"%s\"\n", input);

		//RECORD END TIME
		end = getticks();
		
		results[i] = end-start;
	}
	
	if(write(skt, "END", strlen("END")) < 0){
			perror("write socket");
	} 

	//Close socket
	//------------
	close(skt); 
	
	//Compute avg, stdv and time in ns
	//-------------------------------
	double avg =0, stdv=0, t;
	ticks sum =0;
	
	for(i=0; i<tries; i++){
		sum+= results[i];
	}	
	avg = ((double)sum)/tries;
	
	t = avg/2.4;
	
	for(i=0; i<tries; i++){
		printf("RTT (cycles): %llu\n", results[i]);
		stdv += ((double)results[i]-avg)*((double)results[i]-avg);
		
	}
	stdv = stdv/tries;
	stdv = sqrt(stdv);

	//printf("\nAverage cycles: %lf \nAverage time: %lf ns\nStandart deviation: %lf cycles\n", avg, t, stdv);

	
	return 0;
}

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
	if (argc!=7){  	
		printf("\nERROR! Invalid number of parameters!\n");
		printf("Please use: ./server -port number -msg_size size -samples number_of_samples\n");
		exit(1);
	}
  
	//Set input port and message size
	//-------------------------------
	listen_port = atoi(argv[2]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Valid port range is 1000 to 32768!\n");
		exit(1);
	}
  
	msg_size = atoi(argv[4]);
	if(msg_size < 1){
		printf("\nERROR! Msg size should be set to a positive value!\n");
		exit(1);
	}	
	
	tries = atoi(argv[6]);
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
	char input2[1000];
  	struct sockaddr_in switchAddr;
	int skt;
	char msg[msg_size];
	ticks tear_results[tries];
	ticks setup_results[tries];  
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

		//listen_port++;
		//if(listen_port == 30000) listen_port = 6000;
		
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

	for(i=0; i<tries; i++){ 	
		//Client: set the server address
		//------------------------------
		switchAddr.sin_family = AF_INET;
		switchAddr.sin_port = htons(listen_port); //Set to the port number at which server is expecting to rcv
		switchAddr.sin_addr.s_addr = inet_addr("127.0.0.1");
  
		//Client: connect
		//---------------
		skt = socket(AF_INET,SOCK_STREAM,0);//Create socket
		fcntl(skt, F_SETFL, O_NONBLOCK); //Nonblocking connect

			 
		//RECORD SETUP START TIME
		start = getticks();

		if (connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr)) < 0){
			//perror("send_msg(): ERROR connecting");
		}

		//printf(">> Client: Connecting...");
 
		//Server: wait for new message - Blocking
		//---------------------------------------
		sckTemp = accept(sckListen,(struct sockaddr *)&clientAddr, &sinSize);
		//printf("\n>> Server accepted new connection from %s", inet_ntoa(clientAddr.sin_addr));	  

 		//RECORD SETUP END TIME
		end = getticks();
		setup_results[i] = end-start;

		//Client: Send Message
		//--------------------
		//printf("\n>> Client: Sending msg \"%s\"", msg);
		if(write(skt, msg, strlen(msg)) < 0){
			perror("write socket");
		} 

		//Server: Initialize buffer - read from socket
		//--------------------------------------------
		bzero(input, sizeof (input)); 
		tmp = read(sckTemp, input, sizeof (input));
		//printf("\n>> Server: Received msg \"%s\"", input);

		//printf("\n>> Server: Sending msg \"%s\"", input);
		if(write(sckTemp, input, strlen(input)) < 0){
			perror("write socket");
		} 

		//Client: Read from socket
		//------------------------
		bzero(input2, sizeof (input)); 
		tmp = read(skt, input2, sizeof (input2));
		//printf("\n>> Client: Received msg \"%s\"\n", input2);
	
		//Close sockets
		//-------------
		//RECORD TEAR DOWN START TIME
		start = getticks();
		
		close(skt); 
		close(sckTemp);
		//close(sckListen);

		//RECORD TEAR DOWN END TIME
		end = getticks();
		tear_results[i] = end-start;

	}
	
	//Compute avg, stdv and time in ns
	//-------------------------------
	double setup_avg =0, tear_avg=0, setup_stdv=0, tear_stdv=0, setup_t, tear_t;
	ticks setup_sum, tear_sum =0;
	
	for(i=0; i<tries; i++){
		setup_sum+= setup_results[i];
		tear_sum+= tear_results[i];
	}	
	setup_avg = ((double)setup_sum)/tries;
	tear_avg = ((double)tear_sum)/tries;
	
	setup_t = setup_avg/3.5;
	tear_t = tear_avg/3.5;
	printf("Setup:\tAverage cycles: %lf\n", setup_avg);
	printf("Tear:\tAverage cycles: %lf\n", tear_avg);

	return 0;
}

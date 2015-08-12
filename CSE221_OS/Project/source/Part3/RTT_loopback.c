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

int
main(int argc, char *argv[]){
	//Benchmark variables
	int listen_port, msg_size, tries;
	//needed structures
	struct sockaddr_in serverAddr, clientAddr, switchAddr;
	socklen_t sinSize;
	//socket structures
	int sckTemp, sckListen, skt;
	//temp variables	
	int i, j, k, tmp;
	//Message buffers
	char input[1000], input2[1000], msg[msg_size];
		
	//initialize values (match ping in msg size)
	listen_port = 4001;
	msg_size = 64; //64 bytes like ping
	tries = 10000;
	
	//benchmark results 
	ticks results[tries];  
	ticks start, end;  

	for(i=0; i<msg_size; i++){
		msg[i] = 'a';
	}

	
	//Server: assign socket - Binding
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
	if(listen(sckListen,BACKLOG)<0){
		perror("listen");
		exit(1);
	}

	
	//Client: set the server address
	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); //Set to the port number at which server is expecting to rcv
	switchAddr.sin_addr.s_addr = inet_addr("127.0.0.1");
  
	//Client: connect
	skt = socket(AF_INET,SOCK_STREAM,0);//Create socket
	fcntl(skt, F_SETFL, O_NONBLOCK); //Nonblocking connect

	if (connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr)) < 0){
		//perror("send_msg(): ERROR connecting");
	}

    //Server: wait for new message - Blocking
    sckTemp = accept(sckListen,(struct sockaddr *)&clientAddr, &sinSize);
    
	for(i=0; i<tries; i++){ 
	 
		//RECORD START TIME
		start = getticks();

		//Client: Send Message
		if(write(skt, msg, strlen(msg)) < 0){
			perror("write socket");
		} 

		//Server: Initialize buffer - read from socket
		bzero(input, sizeof (input)); 
		tmp = read(sckTemp, input, sizeof (input));

		//Server: reply
		if(write(sckTemp, input, strlen(input)) < 0){
			perror("write socket");
		} 

		//Client: Read from socket
		bzero(input2, sizeof (input)); 
		tmp = read(skt, input2, sizeof (input2));
	
		//RECORD END TIME
		end = getticks();

		results[i] = end-start;
	}  
	
	//Close sockets
	close(skt); 
	close(sckTemp);
	
	
	//Print results
	double avg =0;
	ticks sum =0;
	
	for(i=0; i<tries; i++){
		sum+= results[i];
	}	
	avg = ((double)sum)/tries;

	printf ("Avg time (cycles): %lf\n", avg);
	
	return 0;
}

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

//------------------------------------------------------------------------------------------------
//Constants used
//------------------------------------------------------------------------------------------------
#define BACKLOG 50

//------------------------------------------------------------------------------------------------
// Main function 
//------------------------------------------------------------------------------------------------
int
main(int argc, char *argv[]){
	//Variables
	//---------
	int listen_port;
	int msg_size;
	struct sockaddr_in serverAddr;
	struct sockaddr_in clientAddr;
 
	//Check input parameters
	//----------------------
	if (argc!=3){  	
		printf("\nERROR! Invalid number of parameters!\n");
		printf("Please use: ./server -port number\n");
		exit(1);
	}
  
	//Set input port and message size
	//-------------------------------
	listen_port = atoi(argv[2]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Valid port range is 1000 to 32768!\n");
		exit(1);
	}
  
	//Variables
	//---------
	int sckTemp;
	int sckListen;
	socklen_t sinSize;
	char input[10000];
	char response[100];
	int i, j, k;
	int tmp;
  	struct sockaddr_in switchAddr;
	
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

	 
    //Server: wait for new message - Blocking
	//---------------------------------------
    sckTemp = accept(sckListen,(struct sockaddr *)&clientAddr, &sinSize);
    
	//printf(">> Server accepted new connection from %s", inet_ntoa(clientAddr.sin_addr));	  
 
 
	while (1){
		//Server: Initialize buffer - read from and send to socket
		//--------------------------------------------------------
		bzero(input, sizeof (input)); 
		tmp = read(sckTemp, input, sizeof (input));
		if(!strcmp(input, "END")) break;
		//printf("\n>> Server: Received msg \"%s\"", input);
		//printf("\n>> Server: Sending msg \"%s\"\n", input);
		if(write(sckTemp, input, strlen(input)) < 0){
			perror("write socket");
		}
	}

	//Close socket
	//------------
	close(sckTemp);

	return 0;
}

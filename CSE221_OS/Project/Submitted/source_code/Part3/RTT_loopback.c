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
	int listen_port, msg_size, tries;
	struct sockaddr_in addr_server, addr_client, switchAddr;
	socklen_t sinSize;
	int sckTemp, sckListen, skt;
	int i, j, k, tmp;
	char input[1000], input2[1000], msg[msg_size];
		
	//initialize values (match ping in msg size)
	listen_port = 4001;
	msg_size = 56; //56 bytes like ping
	tries = 10000;
	
	//benchmark results 
	ticks results[tries];  
	ticks start, end;  

	for(i=0; i<msg_size; i++){
		msg[i] = 'a';
	}

	
	sinSize = sizeof(struct sockaddr_in);  
	sckListen = socket(AF_INET,SOCK_STREAM,0);
	addr_server.sin_family = AF_INET;
	addr_server.sin_port = htons(listen_port);
	addr_server.sin_addr.s_addr = htonl(INADDR_ANY);
  
	if(bind(sckListen,(struct sockaddr *)&addr_server,sizeof(struct sockaddr))<0){
		perror("bind");
		exit(1);
	}

	if(listen(sckListen,50)<0){
		perror("listen");
		exit(1);
	}

	switchAddr.sin_family = AF_INET;
	switchAddr.sin_port = htons(listen_port); 
	switchAddr.sin_addr.s_addr = inet_addr("127.0.0.1");
  
	//Client: connect
	skt = socket(AF_INET,SOCK_STREAM,0);
	fcntl(skt, F_SETFL, O_NONBLOCK);

	connect(skt, (struct sockaddr *)&switchAddr, sizeof(switchAddr))

    sckTemp = accept(sckListen,(struct sockaddr *)&addr_client, &sinSize);
    
	for(i=0; i<tries; i++){ 
	 
		start = getticks();

		if(write(skt, msg, strlen(msg)) < 0){
			perror("write socket");
		} 

		bzero(input, sizeof (input)); 
		tmp = read(sckTemp, input, sizeof (input));

		if(write(sckTemp, input, strlen(input)) < 0){
			perror("write socket");
		} 

		bzero(input2, sizeof (input)); 
		tmp = read(skt, input2, sizeof (input2));
	
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

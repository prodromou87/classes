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
	int tries;
	int msg_size = 1000000;
	struct sockaddr_in addr_serv;
	struct sockaddr_in addr_client;
	int sckTemp;
	int sckListen;
	socklen_t sinSize;
	char input[msg_size];
	int i, j, k;
	int tmp;
  	struct sockaddr_in switchAddr;
 
	//Check input parameters
	if (argc!=3){  	
		printf("USAGE: ./BW_server <port> <samples>\n");
		exit(1);
	}
  
	listen_port = atoi(argv[1]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nERROR! Valid port range is 1000 to 32768!\n");
		exit(1);
	}
	
	tries = atoi(argv[2]); 
	if(tries<1){
		printf("\nERROR! Samples should be a positive number!\n");
		exit(1);
	}
  

	ticks results[tries];  
	ticks start, end;  
	
	sinSize = sizeof(struct sockaddr_in);  
	sckListen = socket(AF_INET,SOCK_STREAM,0);
	addr_serv.sin_family = AF_INET;
	addr_serv.sin_port = htons(listen_port);
	addr_serv.sin_addr.s_addr = htonl(INADDR_ANY);
    
	if(bind(sckListen,(struct sockaddr *)&addr_serv,sizeof(struct sockaddr))<0){
		exit(1);
	}

	if(listen(sckListen,50)<0){
		exit(1);
	}
	 
    sckTemp = accept(sckListen,(struct sockaddr *)&addr_client, &sinSize);
     
	
	for(msg_size = 4096; msg_size<=0.5*1024*1024; msg_size+=16384){
	    for (i=0; i<tries; i++) results[i]=0;

	    for(i=0; i<tries; i++){ 
		    bzero(input, msg_size); 
	
		    tmp=0;
		    while(tmp<msg_size){
			    start = getticks();
			    tmp+=read(sckTemp, input, msg_size-tmp);
			    end = getticks();
			
			    results[i] += end-start;
		    }
	    }  
	
	    double avg =0, bw=0;
	    ticks sum =0;
	
	    for(i=0; i<tries; i++){
	        sum += results[i];
	    }	
	    avg = sum / (double)(tries);
	    bw = (msg_size / (double)(1024*1024)) / (double)(avg / (double)(3500000000)); //=> Mbytes / sec

	    printf("Size: %d KB Peak BW: %lf MB/s\n", msg_size / 1024, bw);
	}
	close(sckTemp);

	return 0;
}

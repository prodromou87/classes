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

int
main(int argc, char *argv[]){
	int listen_port;
	int msg_size;
	struct sockaddr_in addr_serv;
	struct sockaddr_in addr_client;
    int sckTemp;
	int sckListen;
	socklen_t sinSize;
	char input[10000];
	char response[100];
	int i, j, k;
	int tmp;
  	struct sockaddr_in switchAddr;
 
	if (argc!=3){  	
		printf("Usage: ./CON_server <port>\n");
		exit(1);
	}
  
	listen_port = atoi(argv[1]); 
	if(listen_port<1024 || listen_port>32768){
		printf("\nInvalid port\n");
		exit(1);
	}
  
	sinSize = sizeof(struct sockaddr_in);  
	sckListen = socket(AF_INET,SOCK_STREAM,0);
	addr_serv.sin_family = AF_INET;
	addr_serv.sin_port = htons(listen_port);
	addr_serv.sin_addr.s_addr = htonl(INADDR_ANY);
  
	if(bind(sckListen,(struct sockaddr *)&addr_serv,sizeof(struct sockaddr))<0){
		perror("bind");
		exit(1);
	} 

	if(listen(sckListen,50)<0){
		perror("listen");
		exit(1);
	}

	while (1){
		sckTemp = accept(sckListen,(struct sockaddr *)&addr_client, &sinSize);
		bzero(input, sizeof (input)); 
		tmp = read(sckTemp, input, sizeof (input));
		if(!strcmp(input, "END")) break;
		if(write(sckTemp, input, strlen(input)) < 0){
			perror("write socket");
		}
		
		close(sckTemp);
	}

	return 0;
}

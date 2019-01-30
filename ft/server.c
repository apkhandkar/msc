#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <arpa/inet.h>
#include <netinet/in.h>

struct cmsg {
  int cm_type;
  int cm_cblk;
  char cm_body[256];
};

struct smsg {
  int sm_type;
  int sm_nblk;
  char sm_body[1024];
};

int main(int argc, char ** argv)
{
  int sockfd, len, n, fd;
  int nblk, lblk_sz;
  int lablk;
  struct sockaddr_in servaddr, cliaddr;
  struct stat st;

  if(argc != 2) {
    printf("usage: server <port>\n");
    exit(0);
  }

  if((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("socket");
    exit(-1);
  }

  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = INADDR_ANY;
  servaddr.sin_port = htons(atoi(argv[1]));


  if((bind(sockfd, (struct sockaddr*)&servaddr, sizeof(servaddr))) < 0) {
    perror("bind");
    exit(-1);
  }

  printf("started server @ port %d\n", atoi(argv[1]));

  struct cmsg * recv_mesg = (struct cmsg*)malloc(sizeof(struct cmsg));
  struct smsg * send_mesg = (struct smsg*)malloc(sizeof(struct smsg));

  while(1) {
    len = sizeof(cliaddr);
    n = recvfrom(sockfd, recv_mesg, sizeof(struct cmsg), 0, (struct sockaddr*)&cliaddr, (socklen_t*)&len);
    
    if(recv_mesg->cm_type == 0) {

      // client is requesting for a file
      printf("client requested %s\n", recv_mesg->cm_body);

      if((fd = open(recv_mesg->cm_body, O_RDONLY)) < 0) {
        fprintf(stderr, "%s couldn't be opened; bad filename/file not found\n", recv_mesg->cm_body);
        send_mesg->sm_type = -1;
      } else {
        if(fstat(fd, &st) < 0) {
          fprintf(stderr, "couldn't get size of %s\n", recv_mesg->cm_body);
          send_mesg->sm_type = -1;
        } else {
          // calculate number of blocks file will be transferred in
          nblk = (int)ceil((long double)st.st_size/1024);
          // calculate size of last block
          lblk_sz = (int)(((int)st.st_size) - 1024*(nblk-1));
          // transmit number of blocks and size of last block to client
          send_mesg->sm_type = 0;
          send_mesg->sm_nblk = nblk; 
          sprintf(send_mesg->sm_body, "%d", lblk_sz);
        }
      }
      // send number of blocks client should expect
      sendto(sockfd, send_mesg, sizeof(struct smsg), 0, (struct sockaddr*)&cliaddr, len);
      printf("sending %s in %d blocks\n", recv_mesg->cm_body, nblk);

    } else if(recv_mesg->cm_type == 1) {

      if(recv_mesg->cm_cblk == nblk) {

        close(fd);
        
        nblk = 0;
        lblk_sz = 0;

      } else {
        if((lseek(fd, 0, SEEK_CUR)/1024) != recv_mesg->cm_cblk) {
      
          // block to be read by server and block client is requesting do not match
          // this might be because the client wants to resume an interrupted download
          // jump to the requested block
          lseek(fd, (recv_mesg->cm_cblk*1024), SEEK_SET); 
          
        } 

        send_mesg->sm_type = 1;
        send_mesg->sm_nblk = recv_mesg->cm_cblk;
        read(fd, send_mesg->sm_body, 1024);
        sendto(sockfd, send_mesg, sizeof(struct smsg), 0, (struct sockaddr*)&cliaddr, len);

      }

    } else if(recv_mesg->cm_type == -1) {

      printf("client encountered error, cancelling transfer\n");
      close(fd);
      nblk = 0;
      lblk_sz = 0;
    } else {
      printf("client: bad request format\n");
    }
  }

  return 0;
}

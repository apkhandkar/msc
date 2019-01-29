#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
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
  int sockfd, len, n;
  struct sockaddr_in servaddr;
  struct cmsg init_msg;
  struct smsg * recv_mesg;


  if((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
    perror("Socket Error");
    exit(-1);
  }

  servaddr.sin_family = AF_INET;
  servaddr.sin_addr.s_addr = INADDR_ANY;
  servaddr.sin_port = htons(atoi(argv[1]));

  len = sizeof(servaddr);

  
  init_msg.cm_type = 0;
  init_msg.cm_cblk = 0;
  strcpy(init_msg.cm_body, argv[2]);

  if(sendto(sockfd, &init_msg, sizeof(struct cmsg), 0, (const struct sockaddr*)&servaddr, len) < 0) {
    perror("sendto Error");
    exit(-1);
  }
  
  recv_mesg = (struct smsg*)malloc(sizeof(struct smsg));

  int got_all = 0;
  int nblk, cblk, lblk_sz;
  struct cmsg * send_mesg = (struct cmsg*)malloc(sizeof(struct cmsg));
  int fd;
  while(!got_all) {
    n = recvfrom(sockfd, recv_mesg, sizeof(struct smsg), 0, (struct sockaddr*)&servaddr, (socklen_t*)&len);
    
    if(recv_mesg->sm_type < 0) {
      switch(recv_mesg->sm_type) {
        case -1:  fprintf(stderr, "File error: The file doesn't exist on the server, or it cannot be served as of now\n");
                  break;
      }
      exit(-1);
    } else if(recv_mesg->sm_type == 0) {
      nblk = recv_mesg->sm_nblk;
      lblk_sz = atoi(recv_mesg->sm_body);
      cblk = 0;

      // if user specifies filename, save file by that name
      if(argc == 4) {
        fd = open(argv[3], O_CREAT|O_WRONLY, 0777);
        printf("Saving '%s' (~%dKB) as '%s'\n", argv[2], nblk, argv[3]);
      } else if(argc == 3){
        fd = open(argv[2], O_CREAT|O_WRONLY, 0777);
        printf("Fetching '%s' (~%dKB)\n", argv[2], nblk);
      } else {
        printf("Wrong argument format\n");
        exit(-1);
      }
 
      if(fd < 0) {
        fprintf(stderr, "Error: Can't write file to disk\n");
        exit(-1);
      }

      send_mesg->cm_type = 1;
      send_mesg->cm_cblk = cblk;
      sendto(sockfd, send_mesg, sizeof(struct cmsg), 0, (const struct sockaddr*)&servaddr, len);

    } else if(recv_mesg->sm_type == 1) {
      // send acknowledgement
      cblk += 1;
      send_mesg->cm_type = 1;
      send_mesg->cm_cblk = cblk;
      sendto(sockfd, send_mesg, sizeof(struct cmsg), 0, (const struct sockaddr*)&servaddr, len);

      if(cblk==nblk) {
        // received the last block, write the block and exit succesfully!
        write(fd, recv_mesg->sm_body, lblk_sz);
        close(fd);
        exit(0);
      } else {
        // write received block to output file
        write(fd, recv_mesg->sm_body, 1024);
      }
      
    } else {
      printf("Unknown response\n");
    }
  }


  return 0;
}






















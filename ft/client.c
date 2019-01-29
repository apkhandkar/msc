#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
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

  int nblk, cblk, lblk_sz;
  struct cmsg * send_mesg = (struct cmsg*)malloc(sizeof(struct cmsg));
  int fd;
  char temp_fname[256];
  struct stat st;

  while(1) {
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

      // temporarily save the file as <filename>.<extension>.<ftdownload>
      strcpy(temp_fname, argv[2]);
      strcat(temp_fname, ".ftdownload");
      
      // check if temporary file for the same download exists from a previously
      // aborted transfer...
      if(access(temp_fname, F_OK) != -1) {

        // file exists
        // calculate how many full blocks were received, set 'cblk' to that value
        printf("This file seems to have been partially downloaded.\n");
        stat(temp_fname, &st);
        cblk = (int)floor((long double)st.st_size/1024);
        printf("Size downloaded: ~%dKiB. Attempting to resume download...\n", cblk);
        
        // open the file
        if((fd = open(temp_fname, O_WRONLY, 0777)) < 0) {
  
          fprintf(stderr, "Error: Can't open temporary download file\n");
          send_mesg->cm_type = -1;
          exit(-1);

        } else {

          // advance the file pointer to the end of last fully received block:
          if(lseek(fd, (cblk*1024), SEEK_SET) < 0) {

            fprintf(stderr, "Error: Can't resume download from temporary file\n");
            send_mesg->cm_type = -1;

          } else {

            printf("Downloading...\n");
            send_mesg->cm_type = 1;
            send_mesg->cm_cblk = cblk;

          }
        } 

      } else {

        // doesn't exist, go on and create it
        if((fd = open(temp_fname, O_CREAT|O_WRONLY, 0777)) < 0) {

          // file couldn't be opened/created
          fprintf(stderr, "Error: Can't write file to disk\n");
          send_mesg->cm_type = -1;
        
        } else {

          //file was opened succesfully
          printf("Downloading %s (~%dKiB)\n", argv[2], nblk);
          send_mesg->cm_type = 1;
          send_mesg->cm_cblk = cblk;
        
        }
      }
  
      sendto(sockfd, send_mesg, sizeof(struct cmsg), 0, (const struct sockaddr*)&servaddr, len);

      if(send_mesg->cm_type < 0) { 
        exit(-1);
      }





    } else if(recv_mesg->sm_type == 1) {

      if(recv_mesg->sm_nblk == cblk) {
        // server sent the block that was requested
        cblk += 1;
        if(cblk == nblk) {
          // received the last block
          write(fd, recv_mesg->sm_body, lblk_sz);
          rename(temp_fname, argv[argc-1]);
          printf("Download Completed. ");
          printf("Saved as: %s\n", argv[argc-1]);
          close(fd);
  
          exit(0);
        } else {
          write(fd, recv_mesg->sm_body, 1024);
        }
      }

      // if the server sends a block other than the one that was requested, the client
      // won't write the block to disk and keep asking for the correct block    
      send_mesg->cm_type = 1;
      send_mesg->cm_cblk = cblk;
      sendto(sockfd, send_mesg, sizeof(struct cmsg), 0, (const struct sockaddr*)&servaddr, len);

/*
    
      // send acknowledgement
      cblk += 1;
      send_mesg->cm_type = 1;
      send_mesg->cm_cblk = cblk;
      sendto(sockfd, send_mesg, sizeof(struct cmsg), 0, (const struct sockaddr*)&servaddr, len);

      if(cblk==nblk) {
        // received the last block, write the block and exit succesfully!
        write(fd, recv_mesg->sm_body, lblk_sz);
        // rename the temp file to name user specified
        rename(temp_fname, argv[argc-1]);
        printf("Download Completed\n");
        printf("Saved as: %s\n", argv[argc-1]);
        close(fd);

        exit(0);
      } else {
        // write received block to output file
        write(fd, recv_mesg->sm_body, 1024);
      }
*/      
    } else {
      printf("Unknown response\n");
    }
  }


  return 0;
}






















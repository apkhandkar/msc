/*

  ftpack - typedef'd data type and associated functions to
  manipulate and read packets used by nu-ft

  functions:
  
    ftpack_create:  returns  an  ftpack with header  initialised
                    to  specified  'type' and 'size' values, and    
                    with specified 'data', or NULL on failure

    ftpack_psize:   returns size of entire packet

    ftpack_dsize:   returns size of data carried by packet

    ftpack_ptype:   returns  value  under 'type' field of packet
                    header

    ftpack_pdata:   writes 'size' bytes of  data from  packet to
                    'writebuf', returns pointer to 'writebuf'

    ftpack_free:    free the specified packet

  not compiled by default:
  
    ftpack_size:    clumsy  combination  of  'ftpack_psize'  and 
                    'ftpack_dsize'

*/

#ifndef FTPACKET_H
#define FTPACKET_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef OLD_SIZE_FUNC
#define WITHOUT_HEADERS 0
#define WITH_HEADERS    1
ssize_t ftpack_size(void *packet, int mode);
#endif

typedef void * ftpack;

ftpack ftpack_create(int type, void *data, ssize_t size);
ssize_t ftpack_psize(ftpack packet);
ssize_t ftpack_dsize(ftpack packet);
int ftpack_ptype(ftpack packet);
void *ftpack_pdata(ftpack packet, void *writebuf, ssize_t size);
void ftpack_free(ftpack packet);

#endif

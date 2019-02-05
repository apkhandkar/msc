#include "ftpacket.h"

ftpack ftpack_create(int type, void *data, ssize_t size)
{
  void *packdata;

  if((packdata = malloc(sizeof(int) + size + sizeof(ssize_t))) == NULL) {
    return NULL;
  }

  int *ptype = (int*)packdata;
  ssize_t *psize = (ssize_t*)(packdata + sizeof(int));
  void *pdata = (packdata + sizeof(int) + sizeof(ssize_t));

  *ptype = type;
  *psize = size;

  memcpy(pdata, data, size);

  return packdata;
}

/*
ternary operator to obfuscate the code. heheh.
#ifdef OLD_SIZE_FUNC
ssize_t ftpack_size(void *packet, int mode)
{
  return (mode == WITH_HEADERS)
    ? ((*(ssize_t*)(packet + sizeof(int))) + sizeof(int) + sizeof(ssize_t))
    : (mode == WITHOUT_HEADERS)
      ? (*(ssize_t*)(packet + sizeof(int)))
      : -1;
}
#endif
*/

ssize_t ftpack_psize(ftpack packet) 
{ 
  return ((*(ssize_t*)(packet + sizeof(int))) + sizeof(int) + sizeof(ssize_t));
}

ssize_t ftpack_dsize(ftpack packet)
{
  return (*(ssize_t*)(packet + sizeof(int)));
}

int ftpack_ptype(ftpack packet) { return (int)*((int*) packet); }

void *ftpack_pdata(ftpack packet, void *writebuf, ssize_t size)
{
  return memcpy(writebuf, (packet + sizeof(int) + sizeof(ssize_t)), size);
}

void ftpack_free(ftpack packet) { return free(packet); }

#ifdef __cplusplus
extern "C" {
#endif

int socket_getaddrinfo(char* name,char* servicename, void* ret_addrinfo, int mode, int proto);
int socket_sizeof_sockaddr_storage(void);
int socket_create(int mode,int proto);
int socket_getsockname(int s, void* name, int len);
void socket_freeaddrinfo(void* ai);
int socket_bind(int fd,void* name,int len);
int socket_listen(int fd,int l);
int socket_connect(int fd,void* name,int len);
int socket_accept(int fd,void* name,int* len);
void socket_sockaddr_read(void* sp,int *ret_family, void** ret_addr,int* ret_len,int* ret_port);
void socket_addrinfo_read(void* aip,int *ret_family,void** ret_addr,int* ret_len,void** ret_next);
void socket_setnodelay(int fd);
void socket_setreuseaddr(int fd);
int socket_name_family(void* aip);

#ifdef __cplusplus
}
#endif

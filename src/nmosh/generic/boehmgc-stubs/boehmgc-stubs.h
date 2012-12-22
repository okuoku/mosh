
#ifdef __cplusplus
extern "C" {
#endif

void* create_weak_vector(int count);
void* weak_vector_ref(void** wv, int ref);
void weak_vector_set(void** wv, int ref, void* val);
void register_disappearing_link_wv(void** wv,int ref, void* obj);
void register_finalizer(void* obj,void* fn, void* cd);
void register_disappearing_link(void** link, void* obj);
void gcollect(void);
void genable_incremental(void);
int gcurrent_size(void);
int gfree_size(void);
void gset_time_limit(int);

#ifdef __cplusplus
}
#endif


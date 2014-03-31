#ifndef SCHEME_BIGNUM_SHIM_MINI_GMP_H
#define SCHEME_BIGNUM_SHIM_MINI_GMP_H

#ifndef SCHEME_BIGNUM_SHIM_H
#error Do not use this header directly..
#endif

typedef struct {
    mpz_t num;
    mpz_t den;
} tab_mpq_t;

typedef tab_mpq_t mpq_t[1]; 

#define QNUM(x) x[0].num
#define QDEN(x) x[0].den

static inline void mpq_init(mpq_t x){
    mpz_init(QNUM(x));
    mpz_init(QDEN(x));
}

static inline void mpq_clear(mpq_t x){
    mpz_clear(QNUM(x));
    mpz_clear(QDEN(x));
}

static inline void mpq_set_si(mpq_t res, unsigned long int x, unsigned long int y){
}

static inline void mpq_canonicalize(mpq_t x){
}

static inline void mpq_abs(mpq_t res, const mpq_t x){
}

static inline char* mpq_get_str(char* str, int base, const mpq_t x){
    /* Convert to string. num/den or num */
    return 0;
}

static inline double mpq_get_d(const mpq_t x){
    return 0;
}

static inline void mpq_set_d(mpq_t res, double x){
}

static inline void mpq_set_z(mpq_t res, const mpz_t x){
}

static inline int mpq_cmp(const mpq_t x, const mpq_t y){
    return 0;
}

static inline int mpq_cmp_si(const mpq_t x, long int num, long int den){
    return 0;
}

static inline void mpq_add(mpq_t res, const mpq_t x, const mpq_t y){
}

static inline void mpq_sub(mpq_t res, const mpq_t x, const mpq_t y){
}

static inline void mpq_mul(mpq_t res, const mpq_t x, const mpq_t y){
}

static inline void mpq_div(mpq_t res, const mpq_t x, const mpq_t y){
}

static inline int mpq_sgn(const mpq_t x){
    return 0;
}

static inline void mpq_neg(mpq_t res, const mpq_t x){
}

static inline void mpq_set_num(mpq_t res, const mpz_t x){
    mpz_set(QNUM(res), x);
}

static inline void mpq_set_den(mpq_t res, const mpz_t x){
    mpz_set(QDEN(res), x);
}

#define mpq_numref(x) x[0].num
#define mpq_denref(x) x[0].den

#undef QNUM
#undef QDEN

#endif

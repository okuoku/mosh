#ifndef SCHEME_BIGNUM_SHIM_MINI_GMP_H
#define SCHEME_BIGNUM_SHIM_MINI_GMP_H

#include <math.h>

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
    mpz_set_si(QNUM(res), x);
    mpz_set_si(QDEN(res), y);
}

static inline void mpq_canonicalize(mpq_t x){
    /* Calc (gcd num den) and apply it */
    mpz_t gcd;
    mpz_init(gcd);
    mpz_gcd(gcd, QNUM(x), QDEN(x));
    mpz_divexact(QNUM(x), QNUM(x), gcd);
    mpz_divexact(QDEN(x), QDEN(x), gcd);
    mpz_clear(gcd);

    /* Then, flip sign bit for den */
    if(-1 == mpz_sgn(QDEN(x))){
        mpz_neg(QNUM(x), QNUM(x));
        mpz_neg(QDEN(x), QDEN(x));
    }
}

static inline void mpq_set_num(mpq_t res, const mpz_t x){
    mpz_set(QNUM(res), x);
}

static inline void mpq_set_den(mpq_t res, const mpz_t x){
    mpz_set(QDEN(res), x);
}

static inline int mpq_sgn(const mpq_t x){
    const int snum = mpz_sgn(QNUM(x));
    if(0 == snum){ 
        return 0;
    }
    const int sden = mpz_sgn(QDEN(x));
    if(1 == snum){
        return sden;
    }else{
        return -sden;
    }
}

static inline void mpq_neg(mpq_t res, const mpq_t x){
    if(-1 == mpq_sgn(x)){
        mpz_neg(QNUM(res), QNUM(x));
    }
}

static inline void mpq_abs(mpq_t res, const mpq_t x){
    if(-1 == mpq_sgn(x)){
        /* Flip it */
        mpq_neg(res, x);
    }else{
        /* Copy it */
        mpq_set_num(res, QNUM(x));
        mpq_set_den(res, QDEN(x));
    }
}

static inline char* mpq_get_str(char* str, int base, const mpq_t x){
    /* Convert to string. num/den or num */
    /* FIXME: Implement it */
    return 0;
}

static inline double mpq_get_d(const mpq_t x){
    /* FIXME: TOO INEXACT! */
    double n,d;
    n = mpz_get_d(QNUM(x));
    d = mpz_get_d(QDEN(x));
    return n/d;
}

static inline void mpq_set_d(mpq_t res, double x){
    /* FIXME: Implement it */
}

static inline void mpq_set_z(mpq_t res, const mpz_t x){
    mpq_set_num(res, x);
    mpz_set_ui(QDEN(res), 1);
}

static inline void mpq_add(mpq_t res, const mpq_t x, const mpq_t y){
    mpz_t tmp1;
    mpz_t tmp2;
    mpz_init(tmp1);
    mpz_init(tmp2);
    mpz_mul(tmp1, QNUM(x), QDEN(y));
    mpz_mul(tmp2, QDEN(x), QNUM(y));
    mpz_add(QNUM(res), tmp1, tmp2);
    mpz_clear(tmp1);
    mpz_clear(tmp2);
    mpz_mul(QDEN(res), QDEN(x), QDEN(y));
}

static inline void mpq_sub(mpq_t res, const mpq_t x, const mpq_t y){
    mpz_t tmp1;
    mpz_t tmp2;
    mpz_init(tmp1);
    mpz_init(tmp2);
    mpz_mul(tmp1, QNUM(x), QDEN(y));
    mpz_mul(tmp2, QDEN(x), QNUM(y));
    mpz_sub(QNUM(res), tmp1, tmp2);
    mpz_clear(tmp1);
    mpz_clear(tmp2);
    mpz_mul(QDEN(res), QDEN(x), QDEN(y));
}

static inline void mpq_mul(mpq_t res, const mpq_t x, const mpq_t y){
    mpz_mul(QNUM(res), QNUM(x), QNUM(y));
    mpz_mul(QDEN(res), QDEN(x), QDEN(y));
}

static inline void mpq_div(mpq_t res, const mpq_t x, const mpq_t y){
    mpz_mul(QNUM(res), QNUM(x), QDEN(y));
    mpz_mul(QDEN(res), QDEN(x), QNUM(y));
}

static inline int mpq_cmp(const mpq_t x, const mpq_t y){
    /* Shortcut: See sign */
    const int sx = mpq_sgn(x);
    const int sy = mpq_sgn(y);
    if(sx < sy){
        return -1;
    }
    if(sx > sy){
        return 1;
    }
    /* Do actual comparison */
    mpq_t tmp;
    mpq_init(tmp);
    mpq_sub(tmp, x, y);
    const int ss = mpq_sgn(tmp);
    mpq_clear(tmp);

    return ss;
}

static inline int mpq_cmp_si(const mpq_t x, long int num, long int den){
    /* FIXME: Implement short cut */
    mpq_t tmp;
    mpq_init(tmp);
    mpq_set_si(tmp, num, den);
    const int ss = mpq_cmp(x, tmp);
    mpq_clear(tmp);

    return ss;
}



#define mpq_numref(x) x[0].num
#define mpq_denref(x) x[0].den

#undef QNUM
#undef QDEN

#endif

#ifndef SCHEME_BIGNUM_SHIM_CRYPTOPP_H
#define SCHEME_BIGNUM_SHIM_CRYPTOPP_H

#ifndef SCHEME_BIGNUM_SHIM_H
#error Do not use this header directly..
#endif

#include <math.h>
#include <float.h>
#include <integer.h>

typedef void* (mpshim_alloc_fn(size_t size));
typedef void* (mpshim_realloc_fn(void* ptr, size_t oldsiz, size_t newsiz));
typedef void (mpshim_free_fn(void* ptr, size_t siz));

static inline void mp_set_memory_functions(mpshim_alloc_fn* a,
                                           mpshim_realloc_fn* b,
                                           mpshim_free_fn* c){
    /* Nothing to do */
}

typedef CryptoPP::Integer mpz_t[1];
typedef size_t mp_bitcnt_t;

/* alloc */
static inline void mpz_clear(mpz_t x){
    delete x;
}

static inline void mpz_init(mpz_t x){
    x = new CryptoPP::Integer();
}

/* Set */
static inline void mpz_set(mpz_t res, const mpz_t x){
    res[0] = x[0];
}


static inline void mpz_set_d(mpz_t res, double x){
    double m;
    int e;
    m = frexp(x, &e);
    /* Bias with DBL_MANT_DIG to extract integer portion of x */
    m = ldexp(m, DBL_MANT_DIG);
    e -= DBL_MANT_DIG;

    res[0] = (long)m;
    if(e>0){
        res[0] <<= e;
    }else{
        res[0] >>= -e;
    }
}

static inline void mpz_set_si(mpz_t res, signed long int x){
    res[0] = x;
}

static inline void mpz_set_ui(mpz_t res, unsigned long int x){
    CryptoPP::lword t = x;
    res[0] = CryptoPP::Integer(CryptoPP::Integer::POSITIVE, t);
}

static inline void mpz_init_set(mpz_t res, const mpz_t x){
    res = new CryptoPP::Integer(x[0]);

}
static inline void mpz_init_set_si(mpz_t res, signed long int x){
    res = new CryptoPP::Integer(x);
}
static inline void mpz_init_set_ui(mpz_t res, unsigned long int x){
    CryptoPP::lword t = x;
    res = new CryptoPP::Integer(CryptoPP::Integer::POSITIVE, t);
}
static inline void mpz_init_set_str(mpz_t res, const char* str, int base){
    /* NB: Base == 10 */
    res[0] = CryptoPP::Integer(str);
}


/* Get */
static inline signed long int mpz_get_si(const mpz_t x){
    return x[0].ConvertToLong();
}

static inline unsigned long int mpz_get_ui(const mpz_t x){
    if(sizeof(unsigned long) == 4){
        return (unsigned long)(x[0].GetBits(0,32));
    }else{
        return (unsigned long)(x[0].GetBits(0,64));
    }
}

/* Arith */
static inline void mpz_add(mpz_t res, const mpz_t x, const mpz_t y){
    res[0] = x[0] + y[0];
}

static inline void mpz_add_ui(mpz_t res, const mpz_t x, unsigned long int y){
    res[0] = x[0] + CryptoPP::Integer(CryptoPP::Integer::POSITIVE, y);
}

static inline void mpz_sub(mpz_t res, const mpz_t x, const mpz_t y){
    res[0] = x[0] - y[0];
}

static inline void mpz_mul(mpz_t res, const mpz_t x, const mpz_t y){
    res[0] = x[0] * y[0];
}

static inline void mpz_mul_2exp(mpz_t res, const mpz_t x, mp_bitcnt_t y){
    res[0] = x[0] << y;
}

static inline void mpz_neg(mpz_t res, const mpz_t x){
    res[0] = -x[0];
}

static inline void mpz_abs(mpz_t res, const mpz_t x){
    res[0] = x[0].AbsoluteValue();
}

/* Div */
/*   Floor */
static inline void mpz_fdiv_q(mpz_t q, const mpz_t n, const mpz_t d){
    CryptoPP::Integer rem;
    CryptoPP::Integer::Divide(rem, q[0], n[0], d[0]);
    if(rem.IsZero()){
        /* Exact. Do nothing */
    }else if(q[0].IsNegative()){
        /* Downward */
        q[0]--;
    }else{
        /* Positive rem. Do nothing */
    }
}

static inline void mpz_fdiv_q_2exp(mpz_t q, const mpz_t n, mp_bitcnt_t d){
    q[0] = n[0] >> d;
}

/*   Truncate */
static inline void mpz_tdiv_q(mpz_t q, const mpz_t n, const mpz_t d){
    q[0] = n[0]/d[0];
}
static inline void mpz_tdiv_r(mpz_t r, const mpz_t n, const mpz_t d){
    r[0] = n[0].Modulo(d[0]);
}

/*   Ceil */
static inline void mpz_cdiv_q(mpz_t q, const mpz_t n, const mpz_t d){
    CryptoPP::Integer rem;
    CryptoPP::Integer::Divide(rem, q[0], n[0], d[0]);
    if(rem.IsZero()){
        /* Exact. Do nothing */
    }else{
        /* Upward */
        q[0]++;
    }
}

/*   Exact */
static inline void mpz_divexact(mpz_t q, const mpz_t n, const mpz_t d){
    /* Alias to truncate */
    mpz_tdiv_q(q,n,d);
}

/* Root */
static inline void mpz_sqrt(mpz_t res, const mpz_t x){
    res[0] = x[0].SquareRoot();
}

/* Num */
static inline void mpz_gcd(mpz_t res, const mpz_t x, const mpz_t y){
    res[0] = CryptoPP::Integer::Gcd(x[0], y[0]);
}

/* Comp */
static inline int mpz_cmp(const mpz_t x, const mpz_t y){
    return x[0].Compare(y[0]);
}

static inline int mpz_cmp_ui(const mpz_t x, unsigned long int y){
    return x[0].Compare(CryptoPP::Integer(CryptoPP::Integer::POSITIVE, y));
}
static inline int mpz_cmp_si(const mpz_t x, signed long int y){
    return x[0].Compare(CryptoPP::Integer(y));
}
static inline int mpz_sgn(const mpz_t x){
    if(x[0].IsZero()){
        return 0;
    }else if(x[0].IsNegative()){
        return -1;
    }else{
        return 1;
    }
}

/* Exp */
static inline void mpz_pow_ui(mpz_t res, const mpz_t base, unsigned long int e){
    CryptoPP::Integer tmp = 1;
    for(unsigned long int h = 0; h != e; h++){
        tmp *= tmp;
    }
    res[0] = tmp;
}

/* Bits */
/*  MAPBITS2 : Map 2 __positive__ bignum */
#define MAPBITS2(out,x,y,op) \
    do{\
        const size_t xlen = x.ByteCount(); \
        const size_t ylen = y.ByteCount(); \
        const size_t outlen = (xlen > ylen) ? xlen : ylen; \
        out = 0; \
        for(size_t i = 0; i!=outlen; i++){ \
            const unsigned int xx = (i<xlen) ? x.GetByte(i) : 0; \
            const unsigned int yy = (i<ylen) ? y.GetByte(i) : 0; \
            const unsigned int rr = xx op yy; \
            out.SetByte(i, rr); \
        } \
    }while(0);

/*  MAPBITS2N : Map 2 __positive__ bignum with internal negate */
#define MAPBITS2N(out,x,y,op) \
    do{\
        const size_t xlen = x.ByteCount(); \
        const size_t ylen = y.ByteCount(); \
        const size_t outlen = (xlen > ylen) ? xlen : ylen; \
        out = 0; \
        for(size_t i = 0; i!=outlen; i++){ \
            const unsigned int xx = (i<xlen) ? x.GetByte(i) : 0; \
            const unsigned int yy = (i<ylen) ? y.GetByte(i) : 0; \
            const unsigned int rr = xx op ~yy; \
            out.SetByte(i, rr); \
        } \
    }while(0);

static inline CryptoPP::Integer mpshim_com(const CryptoPP::Integer x){
    return -x - 1;
}

static inline void mpz_com(mpz_t res, const mpz_t x){
    /* Bitwise not */
    res[0] = mpshim_com(x[0]);
}

static inline void mpz_xor(mpz_t res, const mpz_t x, const mpz_t y){
    if(x[0].IsZero()){
        res[0] = y[0];
    }else if(y[0].IsZero()){
        res[0] = x[0];
    }else if(x[0] == -1){
        mpz_com(res, y);
    }else if(y[0] == -1){
        mpz_com(res, x);
    }else{
        /* To keep everthing positive value, We do some conversion here.. 
         *     (xor a b)
         *  == (not (xor (not a) b))
         *  == (not (xor a (not b)))
         *  == (xor (not a) (not b)) */
        const int xsgn = mpz_sgn(x);
        const CryptoPP::Integer xxx = (xsgn == -1) ? mpshim_com(x[0]) : x[0];
        const int ysgn = mpz_sgn(y);
        const CryptoPP::Integer yyy = (ysgn == -1) ? mpshim_com(y[0]) : y[0];
        const int outsgn = xsgn * ysgn;
        MAPBITS2(res[0], xxx, yyy, ^);
        if(outsgn == -1){
            mpz_com(res, res);
        }
    }
}

static inline void mpz_and(mpz_t res, const mpz_t x, const mpz_t y){
    if(x[0].IsZero() || y[0].IsZero()){
        res[0] = 0;
    }else if(x[0] == -1){
        res[0] = y[0];
    }else if(y[0] == -1){
        res[0] = x[0];
    }else{
        /* To keep everthing positive value, We do some conversion here.. 
         *     (and a b)
         *  == (not (or (not a) (not b))) */
        const int xsgn = mpz_sgn(x);
        const CryptoPP::Integer xxx = (xsgn == -1) ? mpshim_com(x[0]) : x[0];
        const int ysgn = mpz_sgn(y);
        const CryptoPP::Integer yyy = (ysgn == -1) ? mpshim_com(y[0]) : y[0];
        const int outsgn = xsgn * ysgn;
        if(xsgn == 1 && ysgn == 1){
            MAPBITS2(res[0], xxx, yyy, &);
        }else if(xsgn == -1 && ysgn == 1){
            MAPBITS2N(res[0], yyy, xxx, &);
        }else if(xsgn == 1 && ysgn == -1){
            MAPBITS2N(res[0], xxx, yyy, &);
        }else{
            MAPBITS2(res[0], xxx, yyy, |);
            mpz_com(res, res);
        }
    }
}

static inline void mpz_ior(mpz_t res, const mpz_t x, const mpz_t y){
    if(x[0].IsZero()){
        res[0] = y[0];
    }else if(y[0].IsZero()){
        res[0] = x[0];
    }else if(x[0] == -1){
        res[0] = y[0];
    }else if(y[0] == -1){
        res[0] = x[0];
    }else{
        /* To keep everthing positive value, We do some conversion here.. 
         *     (or a b)
         *  == (not (and (not a) (not b))) */
        const int xsgn = mpz_sgn(x);
        const CryptoPP::Integer xxx = (xsgn == -1) ? mpshim_com(x[0]) : x[0];
        const int ysgn = mpz_sgn(y);
        const CryptoPP::Integer yyy = (ysgn == -1) ? mpshim_com(y[0]) : y[0];
        const int outsgn = xsgn * ysgn;
        if(xsgn == 1 && ysgn == 1){
            MAPBITS2(res[0], xxx, yyy, &);
        }else if(xsgn == -1 && ysgn == 1){
            MAPBITS2N(res[0], xxx, yyy, |);
            mpz_com(res, res);
        }else if(xsgn == 1 && ysgn == -1){
            MAPBITS2N(res[0], yyy, xxx, |);
            mpz_com(res, res);
        }else{
            MAPBITS2(res[0], xxx, yyy, &);
            mpz_com(res, res);
        }
    }
}

#undef MAPBITS2
#undef MAPBITS2N

static inline mp_bitcnt_t mpz_popcount(const mpz_t x){
    if(x[0].IsNegative()){
        /* Upper bits are all 1. Return Infinite */
        return ~(mp_bitcnt_t)0;
    }else if(x[0].IsZero()){
        return 0;
    }else{
        /* FIXME: Implement it */
    }
    return 0;
}
static inline mp_bitcnt_t mpz_scan1(const mpz_t x, mp_bitcnt_t start){
    if(x[0].IsZero()){
        /* We never find 1 here. */
        return ~(mp_bitcnt_t)0;
    }
    /* FIXME: Implement it */
    return 0;
}



/* Misc */
static inline int mpz_fits_slong_p(const mpz_t x){
    return (x[0].IsConvertableToLong()) ? 1 : 0;
}

static inline int mpz_even_p(const mpz_t x){
    return (x[0].IsEven()) ? 1 : 0;
}

static inline size_t mpz_sizeinbase(const mpz_t x, int base){
    unsigned int bitcount;
    switch(base){
        /* Short cut */
        case 2:
        case 8:
        case 16:
            /* First, calc bitwidth */
            bitcount = x[0].BitCount();
            switch(base){
                /* base = 2, return bitcount as-is */
                case 2:
                    return bitcount;
                /* otherwise, do ceil */
                case 8:
                    return (bitcount + 2)/3;
                case 16:
                    return (bitcount + 3)/4;
            }
            break;
        /* Fallback, base == 10 */
        default:
            {
                size_t digits = 0;
                CryptoPP::Integer tmp = x[0];
                do {
                    digits++;
                    tmp /= base;
                }while(tmp != 0);
                return digits;
            }
            break;
    }
}

static inline char* mpz_get_str(char* str, int base, const mpz_t x){
    const char* digits = "0123456789abcdef";
    const int sgnx = mpz_sgn(x);
    const size_t width = mpz_sizeinbase(x, base);
    int i = 0;
    CryptoPP::Integer temp = x[0];
    CryptoPP::Integer buf;
    CryptoPP::SecBlock<char> out(width);
    if(sgnx == -1){
        temp.Negate();
    }

    /* Output digits */
    while(!! temp){
        CryptoPP::word digit;
        CryptoPP::Integer::Divide(digit, buf, temp, base);
        out[i++] = digits[digit];
        temp.swap(buf);
    }
    int j = 0;
    int p = 0;
    if(sgnx == -1){
        str[j] = '-';
        j++;
    }
    for(; j!=i ;j++){
        str[j] = out[p++];
    }
    return str;
}

static inline double mpz_get_d(const mpz_t x){
    double d;
    const int sgnx = mpz_sgn(x);
    CryptoPP::lword i;
    const unsigned int bitwidth = x[0].BitCount();
    const int offs = bitwidth - 53;
    i = x[0].GetBits((offs<0)?0:offs, 53);
    if(offs<0){
        d = i;
    }else{
        d = ldexp(i, offs);
    }
    if(sgnx == -1){
        return -d;
    }else{
        return d;
    }
}

/* Import */
static inline void mpz_import(mpz_t res, size_t count, int odr,
                              size_t size, int endian, size_t nails,
                              const void* data){
    /* FIXME: Implement it */
}
static inline void* mpz_export(void* res, size_t* out_count, int odr,
                               size_t size, int endian, size_t nails,
                               const mpz_t x){
    /* FIXME: Implement it */
    return res;
}

#endif

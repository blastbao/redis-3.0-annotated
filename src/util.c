/*
 * Copyright (c) 2009-2012, Salvatore Sanfilippo <antirez at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "fmacros.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <unistd.h>
#include <sys/time.h>
#include <float.h>

#include "util.h"

/* Glob-style pattern matching. */
/*支持glob-style的通配符格式,如*表示任意一个或多个字符,?表示任意字符,[abc]表示方括号中任意一个字母。
 *将给定字符串与给定模式匹配，匹配返回1，否则返回0。
*/
int stringmatchlen(const char *pattern, int patternLen, const char *string, int stringLen, int nocase)
{
    while(patternLen) {
        switch(pattern[0]) {
        case '*': 
            while (pattern[1] == '*') { //模式的当前字符为*，跳过连续的*值后的部分与给定string的某后部分匹配即可（后缀匹配）。  
                pattern++;
                patternLen--;
            }
            //到此，说明模式全为*，当然匹配所有串，返回1（match）
            if (patternLen == 1)
                return 1; 
            
            //到此，说明需要进行连续*之后的匹配，调用自身递归处理。
            while(stringLen) {
                if (stringmatchlen(pattern+1, patternLen-1, string, stringLen, nocase))
                    return 1; /* match */
                string++;
                stringLen--;
            }
            return 0; /* no match */
            break;
        case '?':   //模式的当前字符为？ ，则直接跳过string的当前字符，从下一字符开始判断  
            if (stringLen == 0)
                return 0; /* no match */
            string++;
            stringLen--;
            break;  //跳出switch,重新循环。
        case '[':
        {
            int not, match;

            pattern++;
            patternLen--;
            not = pattern[0] == '^';
            if (not) {
                pattern++;
                patternLen--;
            }
            match = 0;
            while(1) {
                if (pattern[0] == '\\') {
                    pattern++;
                    patternLen--;
                    if (pattern[0] == string[0])
                        match = 1;
                } else if (pattern[0] == ']') {
                    break;
                } else if (patternLen == 0) {
                    pattern--;
                    patternLen++;
                    break;
                } else if (pattern[1] == '-' && patternLen >= 3) {
                    int start = pattern[0];
                    int end = pattern[2];
                    int c = string[0];
                    if (start > end) {
                        int t = start;
                        start = end;
                        end = t;
                    }
                    if (nocase) {
                        start = tolower(start);
                        end = tolower(end);
                        c = tolower(c);
                    }
                    pattern += 2;
                    patternLen -= 2;
                    if (c >= start && c <= end)
                        match = 1;
                } else {
                    if (!nocase) {
                        if (pattern[0] == string[0])
                            match = 1;
                    } else {
                        if (tolower((int)pattern[0]) == tolower((int)string[0]))
                            match = 1;
                    }
                }
                pattern++;
                patternLen--;
            }
            if (not)
                match = !match;
            if (!match)
                return 0; /* no match */
            string++;
            stringLen--;
            break;
        }
        case '\\':
            if (patternLen >= 2) {
                pattern++;
                patternLen--;
            }
            /* fall through */
        default:
            if (!nocase) {
                if (pattern[0] != string[0])
                    return 0; /* no match */
            } else {
                if (tolower((int)pattern[0]) != tolower((int)string[0]))
                    return 0; /* no match */
            }
            string++;
            stringLen--;
            break;
        }
        pattern++;
        patternLen--;
        if (stringLen == 0) {
            while(*pattern == '*') {
                pattern++;
                patternLen--;
            }
            break;
        }
    }
    if (patternLen == 0 && stringLen == 0)
        return 1;
    return 0;
}

int stringmatch(const char *pattern, const char *string, int nocase) {
    return stringmatchlen(pattern,strlen(pattern),string,strlen(string),nocase);
}

/* Convert a string representing an amount of memory into the number of
 * bytes, so for instance memtoll("1Gi") will return 1073741824 that is
 * (1024*1024*1024).
 *
 * On parsing error, if *err is not NULL, it's set to 1, otherwise it's
 * set to 0 */
long long memtoll(const char *p, int *err) {
    const char *u;
    char buf[128];
    long mul; /* unit multiplier */
    long long val;
    unsigned int digits;

    if (err) *err = 0;
    /* Search the first non digit character. */
    u = p;
    if (*u == '-') 
        u++;
    while(*u && isdigit(*u)) 
        u++;
    if (*u == '\0' || !strcasecmp(u,"b")) {
        mul = 1;
    } else if (!strcasecmp(u,"k")) {
        mul = 1000;
    } else if (!strcasecmp(u,"kb")) {
        mul = 1024;
    } else if (!strcasecmp(u,"m")) {
        mul = 1000*1000;
    } else if (!strcasecmp(u,"mb")) {
        mul = 1024*1024;
    } else if (!strcasecmp(u,"g")) {
        mul = 1000L*1000*1000;
    } else if (!strcasecmp(u,"gb")) {
        mul = 1024L*1024*1024;
    } else {
        if (err) *err = 1;
        mul = 1;
    }
    digits = u-p;
    if (digits >= sizeof(buf)) {
        if (err) *err = 1;
        return LLONG_MAX;
    }
    memcpy(buf,p,digits);
    buf[digits] = '\0';
    val = strtoll(buf,NULL,10);
    return val*mul;
}

/* Convert a long long into a string. Returns the number of
 * characters needed to represent the number, that can be shorter if passed
 * buffer length is not enough to store the whole number. */
int ll2string(char *s, size_t len, long long value) {
    char buf[32], *p;
    unsigned long long v;
    size_t l;

    if (len == 0) 
        return 0;
    v = (value < 0) ? -value : value;
    p = buf+31;             //最多存储32位字符，首先将指针指向最低位
    do {
        *p-- = '0'+(v%10);  //把余数转化为字符
        v /= 10;            //除以10，相当于移位
    } while(v);             //直到v为0结束

    if (value < 0)      //处理负数符号
        *p-- = '-';
    p++;

    l = 32-(p-buf);     //获得转换后的字符长度
    if (l+1 > len) 
        l = len-1;      /* Make sure it fits, including the nul term */
    memcpy(s,p,l);
    s[l] = '\0';
    return l;
}

/*数值转换为字符串:我们一般的做法是每次除以10取余，再将余数转换为对应的字符，如此从低位到高位逐位处理。
 *在这作者提供了更好的做法：由于longlong类型是很长的数值，因此每次除以10，逐位处理需要进行很多次，
 *所以采取除以100，每次处理两位，如此一来节约了一半的时间； 
 *另外，作者将所有的两位数00到99，所对应的字符全部先存放到一个字符串表中，
 *后面只需要直接查表取出对应的字符即可，而不需要每次都去转换。这再次大大的提高了效率！
 */
int ll2string_v2(char* dst, size_t dstlen, long long svalue) {  
    //00到99的数字对应的字符串， 直接查表，而不是每次都将整数转换为字符串。 
    static const char digits[201] =  
        "0001020304050607080910111213141516171819"  
        "2021222324252627282930313233343536373839"  
        "4041424344454647484950515253545556575859"  
        "6061626364656667686970717273747576777879"  
        "8081828384858687888990919293949596979899";  
    int negative;  
    unsigned long long value;  
    uint32_t next;  
  
    /* The main loop works with 64bit unsigned integers for simplicity, so 
     * we convert the number here and remember if it is negative. */  
     //把负数转换为正数处理，加上一位的符号  
    if (svalue < 0) {  
        if (svalue != LLONG_MIN) {  
            value = -svalue;  
        } else {  
            value = ((unsigned long long) LLONG_MAX)+1;  
        }  
        negative = 1;  
    } else {  
        value = svalue;  
        negative = 0;  
    }  
  
    uint32_t const length = digits10(value)+negative;   //Check length: value的位数+符号位
    if (length >= dstlen)                               //由于在字符串中末尾需要包含‘\0’，因此所需的字符串的长度至少要比ll长度大1  
        return 0;
  
    //从后往前，先设置"\0"，然后逐2位转换，直到最后1~2位为止。
    next      = length;  
    dst[next] = '\0';  
    next--;     
    while (value >= 100) {                              //由于每次都是处理的除以100的余数，即ll的低两位,因此从字符串的末尾向前开始填充  
        int const i = (value % 100) * 2;                //乘2的原因是定位到数组中的字符对应的下标。
        value /= 100;  
        dst[next]     = digits[i + 1];                      //直接查表，取得对应的字符  
        dst[next - 1] = digits[i];  
        next -= 2;  
    }  
  
    /* Handle last 1-2 digits. */  
    if (value < 10) {                           //如果余数小于10，直接获得相应字符
        dst[next] = '0' + (uint32_t) value;             
    } else {                                    //如果余数大于10，直接查表
        int i = (uint32_t) value * 2;  
        dst[next] = digits[i + 1];  
        dst[next - 1] = digits[i];  
    }  

    //若为负数，在字符串开头加上负号标志  
    if (negative) 
        dst[0] = '-';  
    return length;  
}  
/* Convert a string into a long long. Returns 1 if the string could be parsed
 * into a (non-overflowing) long long, 0 otherwise. The value will be set to
 * the parsed value when appropriate. */
int string2ll(const char *s, size_t slen, long long *value) {
    const char *p = s;
    size_t plen   = 0;
    int negative  = 0;
    unsigned long long v;

    /* Special case: slen is 0 means error occur, return 0. */
    if (plen == slen)
        return 0;

    /* Special case: first and only digit is 0. */
    if (slen == 1 && p[0] == '0') {
        if (value != NULL) *value = 0;
        return 1;
    }

    /* Sign bit detect, if negative, set variable negative = 1*/
    if (p[0] == '-') {
        negative = 1;
        p++; plen++;

        /* Abort on only a negative sign. */
        if (plen == slen)
            return 0;
    }

    /* First digit should be 1-9, otherwise the string should just be 0. */
    if (p[0] >= '1' && p[0] <= '9') {
        v = p[0]-'0';
        p++; plen++;
    } else if (p[0] == '0' && slen == 1) {
        *value = 0;
        return 1;
    } else {
        return 0;
    }

    //每次将V值乘以10，然后将新读出的字符转化为数字加到V上，每次操作检查是否溢出。
    while (plen < slen && p[0] >= '0' && p[0] <= '9') {
        if (v > (ULLONG_MAX / 10))          /* Overflow. v is unsigned long long, so long long is 1 bit less than bits of v*/
            return 0;
        v *= 10;

        if (v > (ULLONG_MAX - (p[0]-'0')))  /* Overflow. */
            return 0;
        v += p[0]-'0';

        p++; plen++;
    }

    /* Return if not all bytes were used. */
    if (plen < slen)    //this means there is some p[i] is not in [0,9], so error occur. 
        return 0;

    if (negative) {
        if (v > ((unsigned long long)(-(LLONG_MIN+1))+1)) /* Overflow. */
            return 0;
        if (value != NULL) *value = -v;
    } else {
        if (v > LLONG_MAX) /* Overflow. */
            return 0;
        if (value != NULL) 
            *value = v;
    }
    return 1;
}

/* Convert a string into a long. Returns 1 if the string could be parsed into a
 * (non-overflowing) long, 0 otherwise. The value will be set to the parsed
 * value when appropriate. */
int string2l(const char *s, size_t slen, long *lval) {
    long long llval;

    if (!string2ll(s,slen,&llval))
        return 0;

    if (llval < LONG_MIN || llval > LONG_MAX)
        return 0;

    *lval = (long)llval;
    return 1;
}

/* Convert a double to a string representation. Returns the number of bytes
 * required. The representation should always be parsable by stdtod(3). */
int d2string(char *buf, size_t len, double value) {
    if (isnan(value)) {
        len = snprintf(buf,len,"nan");
    } else if (isinf(value)) {
        if (value < 0)
            len = snprintf(buf,len,"-inf");
        else
            len = snprintf(buf,len,"inf");
    } else if (value == 0) {
        /* See: http://en.wikipedia.org/wiki/Signed_zero, "Comparisons". */
        if (1.0/value < 0)
            len = snprintf(buf,len,"-0");
        else
            len = snprintf(buf,len,"0");
    } else {
#if (DBL_MANT_DIG >= 52) && (LLONG_MAX == 0x7fffffffffffffffLL)
        /* Check if the float is in a safe range to be casted into a
         * long long. We are assuming that long long is 64 bit here.
         * Also we are assuming that there are no implementations around where
         * double has precision < 52 bit.
         *
         * Under this assumptions we test if a double is inside an interval
         * where casting to long long is safe. Then using two castings we
         * make sure the decimal part is zero. If all this is true we use
         * integer printing function that is much faster. */
        double min = -4503599627370495; /* (2^52)-1 */
        double max = 4503599627370496; /* -(2^52) */
        if (value > min && value < max && value == ((double)((long long)value)))
            len = ll2string(buf,len,(long long)value);
        else
#endif
            len = snprintf(buf,len,"%.17g",value);
    }

    return len;
}

/* Generate the Redis "Run ID", a SHA1-sized random number that identifies a
 * given execution of Redis, so that if you are talking with an instance
 * having run_id == A, and you reconnect and it has run_id == B, you can be
 * sure that it is either a different instance or it was restarted. 
 */
void getRandomHexChars(char *p, unsigned int len) {
    FILE *fp      = fopen("/dev/urandom","r");
    char *charset = "0123456789abcdef";
    unsigned int j;
    //【原型】size_t  fread(void *buffer, size_t size, size_t count, FILE * stream);
    //【参数】buffer为接收数据的地址，size为一个单元的大小，count为单元个数，stream为文件流。
    //【返回值】返回实际读取的单元个数。如果小于count，则可能文件结束或读取出错；可以用ferror()检测是否读取出错，用feof()函数检测是否到达文件结尾。
    if (fp == NULL || fread(p,len,1,fp) == 0) {
        /* If we can't read from /dev/urandom, do some reasonable effort
         * in order to create some entropy, since this function is used to
         * generate run_id and cluster instance IDs */
        char *x = p;
        unsigned int l = len;
        struct timeval tv;
        pid_t pid = getpid();

        /* Use time and PID to fill the initial array. */
        gettimeofday(&tv,NULL);
        if (l >= sizeof(tv.tv_usec)) {
            memcpy(x,&tv.tv_usec,sizeof(tv.tv_usec));
            l -= sizeof(tv.tv_usec);
            x += sizeof(tv.tv_usec);
        }
        if (l >= sizeof(tv.tv_sec)) {
            memcpy(x,&tv.tv_sec,sizeof(tv.tv_sec));
            l -= sizeof(tv.tv_sec);
            x += sizeof(tv.tv_sec);
        }
        if (l >= sizeof(pid)) {
            memcpy(x,&pid,sizeof(pid));
            l -= sizeof(pid);
            x += sizeof(pid);
        }
        /* Finally xor it with rand() output, that was already seeded with
         * time() at startup. */
        for (j = 0; j < len; j++)
            p[j] ^= rand();
    }
    /* Turn it into hex digits taking just 4 bits out of 8 for every byte. */
    for (j = 0; j < len; j++)
        p[j] = charset[p[j] & 0x0F];
    if (fp) fclose(fp);
}




/*
getRandomHexChars()函数为一个redis instance生成一个160bit的runid，该函数每次生成不同的
唯一标识，不同Redis实例之间该runid是不同的，同一个Redis重启以后，其runid和之前的runid也
是不同的。

Linux中的随机数可以从两个特殊的文件中产生，一个是/dev/urandom，另外一个是/dev/random。
他们产生随机数的原理是利用当前系统的熵池来计算出固定一定数量的随机比特，然后将这些比
特作为字节流返回。

熵池就是当前系统的环境噪音，熵指的是一个系统的混乱程度，系统噪音可以通过很多参数来评估，
如内存的使用，文件的使用量，不同类型的进程数量等等。如果当前环境噪音变化的不是很剧烈或
者当前环境噪音很小，比如刚开机的时候，而当前需要大量的随机比特，这时产生的随机数的随机
效果就不是很好了。

这就是为什么会有/dev/urandom和/dev/random这两种不同的文件，后者在不能产生新的随机数时
会阻塞程序，而前者不会（ublock），当然产生的随机数效果就不太好了，这对加密解密这样的应
用来说就不是一种很好的选择。/dev/random会阻塞当前的程序，直到根据熵池产生新的随机字节
之后才返回，所以使用/dev/random比使用/dev/urandom产生大量随机数的速度要慢。*/

void getRandomHexChars_v2(char *p, unsigned int len)
{
    char *charset = "0123456789abcdef";
    unsigned int j;

    /* Global state. */
    static int seed_initialized = 0;
    static unsigned char seed[20];  /* The SHA1 seed, from /dev/urandom. */
    static uint64_t counter = 0;    /* The counter we hash with the seed. */
    
    //从/dev/urandom读取随机字符串生成随机种子，存储到seed里面。
    if (!seed_initialized) {
        FILE *fp = fopen("/dev/urandom", "r");
        if (fp && fread(seed, sizeof(seed), 1, fp) == 1)
            seed_initialized = 1;
        if (fp) 
            fclose(fp);
    }

    //如果从/dev/urandom读取到了随机字符串，则利用SHA算法生成一个id。
    if (seed_initialized) {
        while (len) {
            //len可能并不是20或者40字节的倍数，所以这里要通过循环把p的内容填满
            unsigned char digest[20];
            unsigned int copylen = len > 20 ? 20 : len;

            SHA1_CTX ctx;
            SHA1Init(&ctx);
            SHA1Update(&ctx, seed, sizeof(seed));
            SHA1Update(&ctx, (unsigned char*)&counter, sizeof(counter));
            SHA1Final(digest, &ctx);
            counter++;

            memcpy(p, digest, copylen);
            /* Convert to hex digits. */
            // 把数字转化为可读字符串，只是这里只用了一个字节的后半部分
            for (j = 0; j < copylen; j++) p[j] = charset[p[j] & 0x0F];

            // 移动光标
            len -= copylen;
            p += copylen;
        }
    }
    else {
        // 如果从/dev/urandom读取随机字符串失败，则利用时间和当前进程的id来生成一个随机字符串
        char *x = p;
        unsigned int l = len;
        struct timeval tv;
        pid_t pid = getpid();

        /* Use time and PID to fill the initial array. */
        //先在buf中填充时间的秒和微秒两个部分，然后再补充上进程的id
        gettimeofday(&tv, NULL);
        if (l >= sizeof(tv.tv_usec)) {
            memcpy(x, &tv.tv_usec, sizeof(tv.tv_usec));
            l -= sizeof(tv.tv_usec);
            x += sizeof(tv.tv_usec);
        }
        if (l >= sizeof(tv.tv_sec)) {
            memcpy(x, &tv.tv_sec, sizeof(tv.tv_sec));
            l -= sizeof(tv.tv_sec);
            x += sizeof(tv.tv_sec);
        }
        if (l >= sizeof(pid)) {
            memcpy(x, &pid, sizeof(pid));
            l -= sizeof(pid);
            x += sizeof(pid);
        }

        //再利用随机数进行异或后，转化为16进制可视字符串
        for (j = 0; j < len; j++) {
            p[j] ^= rand();
            p[j] = charset[p[j] & 0x0F];
        }
    }
}

/* Given the filename, return the absolute path as an SDS string, or NULL
 * if it fails for some reason. Note that "filename" may be an absolute path
 * already, this will be detected and handled correctly.
 *
 * The function does not try to normalize everything, but only the obvious
 * case of one or more "../" appearning at the start of "filename"
 * relative path. */
sds getAbsolutePath(char *filename) {
    char cwd[1024];
    sds abspath;
    sds relpath = sdsnew(filename);

    relpath = sdstrim(relpath," \r\n\t");
    if (relpath[0] == '/') return relpath; /* Path is already absolute. */

    /* If path is relative, join cwd and relative path. */
    if (getcwd(cwd,sizeof(cwd)) == NULL) {
        sdsfree(relpath);
        return NULL;
    }
    abspath = sdsnew(cwd);
    if (sdslen(abspath) && abspath[sdslen(abspath)-1] != '/')
        abspath = sdscat(abspath,"/");

    /* At this point we have the current path always ending with "/", and
     * the trimmed relative path. Try to normalize the obvious case of
     * trailing ../ elements at the start of the path.
     *
     * For every "../" we find in the filename, we remove it and also remove
     * the last element of the cwd, unless the current cwd is "/". */
    while (sdslen(relpath) >= 3 &&
           relpath[0] == '.' && relpath[1] == '.' && relpath[2] == '/')
    {
        sdsrange(relpath,3,-1);
        if (sdslen(abspath) > 1) {
            char *p = abspath + sdslen(abspath)-2;
            int trimlen = 1;

            while(*p != '/') {
                p--;
                trimlen++;
            }
            sdsrange(abspath,0,-(trimlen+1));
        }
    }

    /* Finally glue the two parts together. */
    abspath = sdscatsds(abspath,relpath);
    sdsfree(relpath);
    return abspath;
}

/* Return true if the specified path is just a file basename without any
 * relative or absolute path. This function just checks that no / or \
 * character exists inside the specified path, that's enough in the
 * environments where Redis runs. */
int pathIsBaseName(char *path) {
    return strchr(path,'/') == NULL && strchr(path,'\\') == NULL;
}

#ifdef UTIL_TEST_MAIN
#include <assert.h>

void test_string2ll(void) {
    char buf[32];
    long long v;

    /* May not start with +. */
    strcpy(buf,"+1");
    assert(string2ll(buf,strlen(buf),&v) == 0);

    /* Leading space. */
    strcpy(buf," 1");
    assert(string2ll(buf,strlen(buf),&v) == 0);

    /* Trailing space. */
    strcpy(buf,"1 ");
    assert(string2ll(buf,strlen(buf),&v) == 0);

    /* May not start with 0. */
    strcpy(buf,"01");
    assert(string2ll(buf,strlen(buf),&v) == 0);

    strcpy(buf,"-1");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == -1);

    strcpy(buf,"0");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == 0);

    strcpy(buf,"1");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == 1);

    strcpy(buf,"99");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == 99);

    strcpy(buf,"-99");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == -99);

    strcpy(buf,"-9223372036854775808");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == LLONG_MIN);

    strcpy(buf,"-9223372036854775809"); /* overflow */
    assert(string2ll(buf,strlen(buf),&v) == 0);

    strcpy(buf,"9223372036854775807");
    assert(string2ll(buf,strlen(buf),&v) == 1);
    assert(v == LLONG_MAX);

    strcpy(buf,"9223372036854775808"); /* overflow */
    assert(string2ll(buf,strlen(buf),&v) == 0);
}

void test_string2l(void) {
    char buf[32];
    long v;

    /* May not start with +. */
    strcpy(buf,"+1");
    assert(string2l(buf,strlen(buf),&v) == 0);

    /* May not start with 0. */
    strcpy(buf,"01");
    assert(string2l(buf,strlen(buf),&v) == 0);

    strcpy(buf,"-1");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == -1);

    strcpy(buf,"0");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == 0);

    strcpy(buf,"1");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == 1);

    strcpy(buf,"99");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == 99);

    strcpy(buf,"-99");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == -99);

#if LONG_MAX != LLONG_MAX
    strcpy(buf,"-2147483648");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == LONG_MIN);

    strcpy(buf,"-2147483649"); /* overflow */
    assert(string2l(buf,strlen(buf),&v) == 0);

    strcpy(buf,"2147483647");
    assert(string2l(buf,strlen(buf),&v) == 1);
    assert(v == LONG_MAX);

    strcpy(buf,"2147483648"); /* overflow */
    assert(string2l(buf,strlen(buf),&v) == 0);
#endif
}

int main(int argc, char **argv) {
    test_string2ll();
    test_string2l();
    return 0;
}
#endif

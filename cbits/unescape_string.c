#include <stdlib.h>

void bs_json_unescape(unsigned long length, int *error, char *bsIn, char *bsOut);

void private_parse_hex(char *bsIn, int *number, short *error) {
    char a = *bsIn;
    bsIn++;
    char b = *bsIn;
    bsIn++;
    char c = *bsIn;
    bsIn++;
    char d = *bsIn;
    bsIn++;

    char hex[4] = { a, b, c, d };
    char *internalError;
    *number = (int)strtol(hex, &internalError, 16);
    if (*internalError != '\0') {
        *error = 1;
    } else {
        *error = 0;
    }
}

void bs_json_unescape(unsigned long length, int *error, char *bsIn, char *bsOut)
{
    unsigned long ptr = 0;
    while (ptr < length) {
        char ch = *bsIn;
        bsIn++;
        ptr++;
        if (ch == '\\') {
            if (ptr >= length) {
                *error = 1;
                return;
            }
            char nextCh = *bsIn;
            bsIn++;
            ptr++;
            switch (nextCh) {
            case '\\':
                *bsOut = '\\';
                break;
            case '"':
                *bsOut = '"';
                break;
            case '/':
                *bsOut = '/';
                break;
            case 'b':
                *bsOut = '\b';
                break;
            case 'f':
                *bsOut = '\f';
                break;
            case 'n':
                *bsOut = '\n';
                break;
            case 'r':
                *bsOut = '\r';
                break;
            case 't':
                *bsOut = '\t';
                break;
            case 'u':
                if (ptr+3 >= length) {
                    *error = 3;
                    return;
                }
                ptr += 4;
                int number = 0;
                short hexError = 0;
                private_parse_hex(bsIn, &number, &hexError);
                if (hexError == 1) {
                    *error = 4;
                    return;
                }
                if (number < 0xd800 || number > 0xdfff) {
                    *bsOut = (char)number;
                } else if (number <= 0xdbff) {
                    if (ptr+5 >= length) {
                        *error = 5;
                        return;
                    }
                    ptr += 6;
                    if (*bsIn != '\\') {
                        *error = 6;
                        return;
                    }
                    bsIn++;
                    if (*bsIn != 'u') {
                        *error = 7;
                        return;
                    }
                    bsIn++;
                    int numberB = 0;
                    short hexErrorB = 0;
                    private_parse_hex(bsIn, &numberB, &hexErrorB);
                    if (hexErrorB == 1) {
                        *error = 8;
                        return;
                    }
                    if (numberB >= 0xdc00 && numberB <= 0xdfff) {
                        int r = ((number - 0xdc00) << 10) + (numberB - 0xdc00) + 0x10000;
                        *bsOut = (char)r;
                    } else {
                        *error = 9;
                        return;
                    }
                }
                break;
            default:
                *error = 2;
                return;
            }
            bsOut++;
        } else {
            *bsOut = ch;
            bsOut++;
        }
    }
    *bsOut = '\0';
    *error = 0;
}

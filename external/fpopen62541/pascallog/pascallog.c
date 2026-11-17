#include <open62541/plugin/log.h>
#include <open62541/types.h>
//gcc -fPIC -I /home/luca/Datos/open62541/open62541-linux64/include/ -shared -o libua_pascallog.so pascallog.c

#if defined(_WIN32)
#  ifdef __GNUC__
#   define SYM_EXPORT __attribute__ ((dllexport))
#  else
#   define SYM_EXPORT __declspec(dllexport)
#  endif
#else /* non win32 */
# if __GNUC__ || __clang__
#  define SYM_EXPORT __attribute__ ((visibility ("default")))
# endif
#endif
#ifndef SYM_EXPORT
# define SYM_EXPORT /* fallback to default */
#endif

#define LOGBUFSIZE 1024


/* function prototype for pascal logging function */
typedef void (*pascal_log_t) (void *context, UA_LogLevel level, UA_LogCategory category, const char *msg);

/* struct to hold both the original context and the logging function */
typedef struct PascalLogTrampoline {
  pascal_log_t func;
  void * context;
} PascalLogTrampoline;

/*
freepascal doesn't allow to define a callback function with c compatible varargs,
this function formats the message and the args in a buffer then
calls the pascal function without varargs
*/

static void
UA_Log_Pascal_log(void *context, UA_LogLevel level, UA_LogCategory category,
                  const char *msg, va_list args) {
        char logbuf[LOGBUFSIZE];
        if (context != NULL) {
            PascalLogTrampoline *trampoline = context;
            vsnprintf(logbuf, LOGBUFSIZE, msg, args);
            trampoline->func(trampoline->context, level, category, logbuf);
        }
}

static void
UA_Log_Pascal_clear(void *context) {
        free(context);
}

/*
Initializes a logger that stores the callback function and the original
context in a struct, then uses UA_Log_Pascal_log to dispatch the messages
*/
SYM_EXPORT UA_Logger UA_Pascal_logger(void *context, pascal_log_t func) {
        PascalLogTrampoline *trampoline=malloc(sizeof(PascalLogTrampoline));
        trampoline->context = context;
        trampoline->func = func;
        UA_Logger logger = {UA_Log_Pascal_log, (void *)trampoline, UA_Log_Pascal_clear};
        return logger;
}

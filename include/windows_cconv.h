#ifndef WINDOWS_CCONV_H
#define WINDOWS_CCONV_H

#if defined(mingw32_HOST_OS)
#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif
#else
# define WINDOWS_CCONV
#endif

#endif

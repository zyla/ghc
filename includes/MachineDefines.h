
#ifndef INCLUDES_MACHINE_DEFINES
#define INCLUDES_MACHINE_DEFINES

#include "ghcplatform.h"

// Any *_HOST_ARCH macro that is not defined in the above file is now
// explicitly set to zero.

#ifndef aarch64_HOST_ARCH
#define aarch64_HOST_ARCH 0
#endif

#ifndef alpha_HOST_ARCH
#define alpha_HOST_ARCH 0
#endif

#ifndef arm_HOST_ARCH
#define arm_HOST_ARCH 0
#endif

#ifndef arm_HOST_ARCH_PRE_ARMv6
#define arm_HOST_ARCH_PRE_ARMv6 0
#endif

#ifndef arm_HOST_ARCH_PRE_ARMv7
#define arm_HOST_ARCH_PRE_ARMv7 0
#endif

#ifndef i386_HOST_ARCH
#define i386_HOST_ARCH 0
#endif

#ifndef ia64_HOST_ARCH
#define ia64_HOST_ARCH 0
#endif

#ifndef powerpc64_HOST_ARCH
#define powerpc64_HOST_ARCH 0
#endif

#ifndef powerpc64le_HOST_ARCH
#define powerpc64le_HOST_ARCH 0
#endif

#ifndef powerpc_HOST_ARCH
#define powerpc_HOST_ARCH 0
#endif

#ifndef rs6000_HOST_ARCH
#define rs6000_HOST_ARCH 0
#endif

#ifndef sparc_HOST_ARCH
#define sparc_HOST_ARCH 0
#endif

#ifndef x86_64_HOST_ARCH
#define x86_64_HOST_ARCH 0
#endif

#ifndef x86_64_HOST_ARCH
#define x86_64_HOST_ARCH 0
#endif

//------------------------------------------------------------------------------
// Any *_HOST_OS macro that is not defined is now explicitly set to zero.

#ifndef aix_HOST_OS
#define aix_HOST_OS 0
#endif

#ifndef darwin_HOST_OS
#define darwin_HOST_OS 0
#endif

#ifndef dragonfly_HOST_OS
#define dragonfly_HOST_OS 0
#endif

#ifndef freebsd_HOST_OS
#define freebsd_HOST_OS 0
#endif

#ifndef gnu_HOST_OS
#define gnu_HOST_OS 0
#endif

#ifndef haiku_HOST_OS
#define haiku_HOST_OS 0
#endif

#ifndef hpux_HOST_OS
#define hpux_HOST_OS 0
#endif

#ifndef ios_HOST_OS
#define ios_HOST_OS 0
#endif

#ifndef kfreebsdgnu_HOST_OS
#define kfreebsdgnu_HOST_OS 0
#endif

#ifndef linux_HOST_OS
#define linux_HOST_OS 0
#endif

#ifndef mingw32_HOST_OS
#define mingw32_HOST_OS 0
#endif

#ifndef ndefmingw32_HOST_OS
#define ndefmingw32_HOST_OS 0
#endif

#ifndef ndefopenbsd_HOST_OS
#define ndefopenbsd_HOST_OS 0
#endif

#ifndef netbsd_HOST_OS
#define netbsd_HOST_OS 0
#endif

#ifndef openbsd_HOST_OS
#define openbsd_HOST_OS 0
#endif

#ifndef solaris2_HOST_OS
#define solaris2_HOST_OS 0
#endif


#endif

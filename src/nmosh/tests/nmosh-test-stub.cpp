#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "TestingVM.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"
#include "Closure.h"
#include "EqHashTable.h"
#include "Symbol.h"
#include "VM.h"
#include "Gloc.h"
#include "VM-inl.h"

using namespace scheme;

VM* theVM; /* Fake main VM */

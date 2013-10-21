#ifndef WRAPPER_H
#define WRAPPER_H
#include <stdlib.h>
#include "chipmunk.h"
#include "chipmunk_unsafe.h"

// From cpBody.h
void wrBodyUpdateVelocity(cpBody*, cpVect*, cpFloat, cpFloat);
void wrBodyApplyImpulse(cpBody*, cpVect*, cpVect*);
void wrDampedSpring(cpBody*, cpBody*, cpVect*, cpVect*, cpFloat, cpFloat, cpFloat, cpFloat);
void wrBodyLocal2World(cpBody*, cpVect*);
void wrBodyWorld2Local(cpBody*, cpVect*);
void wrBodyApplyForce(cpBody*, cpVect*, cpVect*);

// From cpShape.h
void wrCircleShapeInit(cpCircleShape*, cpBody*, cpVect*, cpFloat);
void wrSegmentShapeInit(cpSegmentShape*, cpBody*, cpVect*, cpVect*, cpFloat);
int wrShapePointQuery(cpShape*, cpVect*);
int wrShapeSegmentQuery(cpShape*, cpVect*, cpVect*, cpSegmentQueryInfo*);

// From cpPolyShape.h
void wrPolyShapeInit(cpPolyShape*, cpBody*, int, cpVect*, cpVect*);

// From various constraints
// Note that we change the argument order to allow easy currying on Haskell side.
void wrPinJointInit(cpVect*, cpVect*, cpPinJoint*, cpBody*, cpBody*);
void wrSlideJointInit(cpFloat, cpFloat, cpVect*, cpVect*, cpSlideJoint*, cpBody*, cpBody*);
void wrPivot1JointInit(cpVect*, cpPivotJoint*, cpBody*, cpBody*);
void wrPivot2JointInit(cpVect*, cpVect*, cpPivotJoint*, cpBody*, cpBody*);
void wrGrooveJointInit(cpVect*, cpVect*, cpVect*, cpGrooveJoint*, cpBody*, cpBody*);
void wrGearJointInit(cpFloat, cpFloat, cpGearJoint*, cpBody*, cpBody*);
void wrDampedSpringInit(cpFloat, cpFloat, cpFloat, cpVect*, cpVect*, cpDampedSpring*, cpBody*, cpBody*);
void wrDampedRotarySpringInit(cpFloat, cpFloat, cpFloat, cpDampedRotarySpring*, cpBody*, cpBody*);
void wrRatchetJointInit(cpFloat, cpFloat, cpRatchetJoint*, cpBody*, cpBody*);
void wrRotaryLimitJointInit(cpFloat, cpFloat, cpRotaryLimitJoint*, cpBody*, cpBody*);
void wrSimpleMotorInit(cpFloat, cpSimpleMotor*, cpBody*, cpBody*);

// From cpArbiter.h
void wrArbiterTotalImpulse(cpArbiter*, cpVect*);
void wrArbiterTotalImpulseWithFriction(cpArbiter*, cpVect*);
void wrArbiterGetNormal(cpArbiter*, cpVect*);

// From cpSpace.h
void wrSpacePointQuery(cpSpace*, cpVect*, cpLayers, cpGroup, cpSpacePointQueryFunc);

// From chipmunk_unsafe.h
void wrCircleShapeSetOffset(cpShape*, cpVect*);
void wrSegmentShapeSetEndpoints(cpShape*, cpVect*, cpVect*);
void wrPolyShapeSetVerts(cpShape*, int, cpVect*, cpVect*);

#endif

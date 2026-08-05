---
title: Lerp, the little formula behind smooth movement
date: 2026-08-05
tags:
  - gamedev
  - math
---

Behind every smooth movement in a 2D game: a player easing toward a target, a camera drifting back after you stop, a health bar draining, there is usually the same formula wearing a different coat. *Linear interpolation*, or **lerp** for short.

## The formula

Given two values $a$ and $b$, lerp gives you a point between them based on a parameter $t$:

$$
\text{lerp}(a, b, t) = a + (b - a) \cdot t
$$

Let's check the edges. When $t = 0$:

$$
a + (b - a) \cdot 0 = a
$$

When $t = 1$:

$$
a + (b - a) \cdot 1 = a + b - a = b
$$

And when $t = 0.5$ you get the midpoint, $(a + b) / 2$. So $t$ is best read as **the fraction of the way from $a$ to $b$**. It's a slider between two endpoints, and $t$ says where the knob is.

The $(b - a)$ part is the *offset* you'd have to travel, and $(b - a) \cdot t$ is the portion of that travel you've completed. Add it to the start and you're there. That's the whole trick: lerp is just *start + progress toward the destination*.

## In 2D

A position is just two values that move together, so the 2D version applies the formula per axis:

$$
\text{lerp}(P_0, P_1, t) = \left( x_0 + (x_1 - x_0) t,\; y_0 + (y_1 - y_0) t \right)
$$
 
Since $x$ and $y$ are independent, anything that's true in one dimension carries over. $t = 0.25$ puts you a quarter of the way between the two points. 

## The one subtlety: time


If you want an object to approach its target by 10% with each frame, you could be tempted to do something like:

```javascript
// Frame-rate dependent update 
sprite.x += (target.x - sprite.x) * 0.1
```

But while the movement may look correct on screen, the approach speed is calculated per frame, not per second. This means that the animation’s speed depends entirely on the frame rate. It will appear slower at 60 frames per second and faster at 144 frames per second. Same code, but a completely different experience depending on the machine.

The fix is to scale the step by delta time (elapsed time since last update):

```javascript
// Frame-rate independent:
const k = 1 - Math.exp(-rate * dt)
sprite.x += (target.x - sprite.x) * k
```

The exponential factor $1 - e^{-r \cdot dt}$ is the exact, frame-rate-independent version of the same idea, set $r$ once and the movement will feel the same at any frame rate.

## Taking it further: easing

Because lerp assumes $t$ moves uniformly, linear motion can feel robotic. The usual trick is to keep lerp as is and remap $t$ through a function first, the most popular being *smoothstep*. Ease-in/out curves, overshoots, and elastic bounces are all just different ways of warping $t$ before it enters the same formula.

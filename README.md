# Nebula Raymarching
A multi-density volumetric CPU renderer which utilises raymarcing and multi-threading. Written in haskell and made for the INF1A FP Competition at the University of Edinburgh

**Nebula containing hydrogen-alpha and oxygen-III**
![Final render 1](images/final1.png)

## Usage
The renderer is built using the `gloss` and `pure-noise` packages and will need to installed before running. The programm also utilises multithreading and will need to be built with the `-threaded` and ran with `+RTS -N -RTS` inorder to take advantage of this optimisatio.

The volume density field and rendering properties can be set in `Main.hs` and `Volume.hs`. There are plans to move these values into a configuration file.

## How it works
The renderer can be broken down into these categories
### Main window thread and worker threads initalization
A foriegn pointer array is used as a render buffer which the window thread will read from and worker threads will write to.

A major optimisation found was that a lock was not need, usually this would cause race conditions and weird render output however each worker is only small and distinct sections of the buffer therefore there will be no race conditions as they operate on different parts of the array.

### Raymarching
1. The program uses orthographic progection so the rays are all paralel and will step along the z axis.
2. It samples the volume at each step which returns a list of density values which represents a different element.
3. For each density a different transfer function is applied and the ouputs are summed together. The scattered light at the point is also calculated and all accumated.
4. The transmission at each point is calculated based on the step size, opacity and the accumated transmission. The final color and transmittion is calculated and is accumulated. The final color at the pixel is the accumlated color and is calculated by $$\text{Color}\times\text{Scatted light} + \text{Emission}$$

![Raymarching diagram](images/raymarch_diagram.png)

### Transfer function
The transfer function defines a color, emmission and opacity for a given density. It heavily relys on different interpolation functions and color ramps.

Transfer for different elements are supplied such as $\text{H}\alpha$ (red/orange), $\text{O}^3$ (blue/cyan) and $\text{S}^2$ (deep red/pink)

### Volume sampling
Mutli-octave 3D fractal perlin noise is used to generate the base density field of the nebula. To give the density field a more flowly/nebula shape the input domain is warped by a downscaled inner noise. For composite clouds (clouds with multiple densities) the warp uses a different seed to give it the same overall shape but different details.

Further processing such as falloff and cutoff values are applied to create the nebula look.

### Light scattering
Light scattering happens at each step in the main raymarching step. At each point a new ray is marched towards the light and accumates the volumes color along the way and the light at the end of the march.

![Light scattering diagram](images/scattering_diagram.png)
> Image taken from [this blog](https://blog.maximeheckel.com/posts/real-time-cloudscapes-with-volumetric-raymarching/) 

It also uses the [Henyey-Greenstien Phase Function](https://omlc.org/classroom/ece532/class3/hg.html) to calculate how much light should be accumlated based on the angle to the light source.
![Phase function diagram](images/phase_diagram.png)
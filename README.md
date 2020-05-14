
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sketcher

<!-- badges: start -->

<!-- badges: end -->

By using the `sketcher` package, you can convert a photo into a line
drawing image.  
Drawing style (lineweight and inclusion/exclusion of shadow/shading) can
be controlled.  
Some examples (source photos and generated sketches) are shown below.

<p>

<img src="notes/face_demo_2000.jpg" width="100%">

</p>

<p>

<img src="notes/others2_2000.jpg" width="100%">

</p>

## Paper

Details of this package are described in the article below:

Tsuda, H. (2020). sketcher: An R package for converting a photo into a
sketch style image.  
<https://psyarxiv.com/svmw5>

## Dependencies

Mac OS X users need to install XQuartz (<https://www.xquartz.org/>).

## Installation

Please use the `devtools` package to install the `sketcher` package.

``` r
devtools::install_github("tsuda16k/sketcher")
```

(**NOTE:** The `sketcher` package is currently under review by CRAN, so
install.packages(“sketcher”) command does not work.)

Then, attach the package.

``` r
library(sketcher)
```

## Example image

The `sketcher` package has a built-in image, which is useful when you
want to try sketch effects right away. The image data is named `face`.

Internally, this is just a numeric array of size 600 x 460 x 3
\[y-coordinate, x-coordinate, color channel\]. Each element of the array
represents a pixel value, which can range between 0 and 1.

``` r
dim(face)
#> [1] 600 460   3
```

To plot an image, use the `plot()` function.

``` r
plot(face)
```

<img src="notes/face.png" width="40%">

(In examples below, I actually used
<a href="notes/sketcher_face.jpg">this image</a>, which has higher
resolution than the image provided by the package. Sketch results will
be similar, but in general aesthetically more pleasing results can be
obtained with images of higher resolution, at the cost of processing
time.)

## Load an image

To load your own image, use the `im_load()` function.

``` r
im = im_load("path/to/your/image.jpg")
plot(im)
```

The jpg, png, and bmp formats are supported.

You can load an image from the web (URL). For
example,

``` r
im = im_load("https://raw.githubusercontent.com/tsuda16k/sketcher/master/notes/sketcher_face.jpg")
```

will load a high resolution version of the face image noted above
(compare `dim(face)` and `dim(im)`).

## Apply the sketch effect

The built-in face image is used in the following examples.  
For consistency purposes, the `face` image is labeled as `im`.

``` r
im = face
```

Use the `sketch()` function to apply the sketch effect to an image.

``` r
im2 = sketch(im) # may take some seconds
plot(im2)
```

<p>

<img src="notes/sketch.png" width="50%">

</p>

That’s all.

The `sketch()` function has several parameters to control the style of
sketch.

## Arguments of the sketch() function

A table of arguments of the `sketch()`
function:

| Argument   | Meaning                  | Value                  | Default                           |
| :--------- | :----------------------- | :--------------------- | :-------------------------------- |
| im         | An input image           | image                  |                                   |
| style      | Sketch style             | 1 or 2                 | 1                                 |
| lineweight | Strength of lines        | a numeric, \>= 0.3     | 1                                 |
| smooth     | Smoothness of texture    | an integer, \>= 0      | 1                                 |
| gain       | Gain parameter           | a numeric betw 0 and 1 | 0.02                              |
| contrast   | Contrast parameter       | a numeric, \>= 0       | 20 (for style1) or 4 (for style2) |
| shadow     | Shadow threshold         | a numeric betw 0 and 1 | 0.0                               |
| max.size   | Max resolution of output | an integer, \> 0       | 2048                              |

The default is `sketch(im, style = 1, lineweight = 1, smooth =
lineweight, gain = .02, contrast = NULL, shadow = 0, max.size = 2048)`.

  - im: an image, obtained by using the `im_load()` function.
  - style: while style 1 focuses on edges, style 2 also retains shading.
  - lineweight: as the name suggests. set a numeric value equal to or
    larger than 0.3.
  - smooth: noise/blob smoother. set an integer value equal to or larger
    than 0.
  - gain: this parameter may be useful for noise reduction in dim
    region.
  - contrast: contrast of the sketch image is adjusted by this
    parameter.
  - shadow: if given a value larger than 0 (e.g., 0.3), shadows are
    added to sketch.
  - max.size: the size (image resolution) of output sketch is
    constrained by this parameter. if the input image has a very high
    resolution, such as 20000 x 10000 pixels, sketch processing will
    take a long time. In such cases, the algorithm first downscales the
    input image to 2048 x 1024 pixels, in this case, and then apply the
    sketch effect.

The effects of these parameters on sketch appearances are described in
detail below.

## Saving the image

Use the `im_save()` function to save an image.

``` r
im = face
im2 = sketch(im, style = 1, lineweight = 1, shadow = 0.3)

# myimage.png is saved in the current working directory
im_save(im2, name = "myimage", path = getwd())

# newimg.jpg is saved to a specified directory
im_save(im2, name = "newimg", path = "set/your/path", format = "jpg", quality = .95)
```

By default, an image is saved in the png format.  
When using `format = "jpg"`, you can set the quality of jpg compression
(default is 0.95).

## The effects of sketch parameters

### \- style and lineweight

The most important parameters of the `sketch()` function are `style` and
`lineweight`.

While style 1 is good at extracting fine edges, style 2 retains shading,
as shown in the figure. Note that the shading gets more salient when
`lineweight` is given a larger value.

<p>

<img src="notes/style_line_1200.jpg" width="80%">

</p>

### \- smooth

The `smooth` parameter controls the degree of smoothness of image
texture. By increasing the `smooth` value, fine-scale edges, noises, and
blobs are eliminated (see the figure below).

In most cases, aesthetically pleasing results will be obtained when
`smooth` value is equal to or larger than `lineweight`. If `smooth` is
not given in the `sketch()` function, `smooth` is assigned with the same
value as
`lineweight`.

<p>

<a href="https://raw.githubusercontent.com/tsuda16k/sketcher/master/notes/line_smooth_2100.jpg"><img src="notes/line_smooth_1500.jpg" width="100%"></a>

</p>

### \- gain

A constant value (gain) is added to an input image before the extraction
of edges to produce a sketch. A sketch can be very noisy when an input
image has dark/dim regions. In such cases, increasing the gain, such as
`gain = 0.2` or `gain = 0.3`, may reduce the noise. In most cases,
however, you don’t have to care about this parameter.

### \- contrast

By increasing the `contrast` parameter, a sketch is darkened. When a
sketch appears whitish, you may need to increase the `contrast` value.

<p>

<img src="notes/contrast_900.jpg" width="100%">

</p>

### \- shadow

Shadow can be added to a sketch by using the `shadow` parameter.  
By default, the sketch function does not include shadow (shadow = 0). In
many cases, however, adding shadow will be needed to produce a
reasonable result (described later).

<p>

<img src="notes/shadow_900.jpg" width="100%">

</p>

## Tips for successful sketching

For some images, good results may be obtained with the default
parameters of the sketch function. However, in many cases, the default
sketch will produce an unsatisfactory result. Here I show some cases
where the default sketch fails, and how to fix it.

### Case 1

Outline is missing and texture is lacking in the default sketch. By
using style 2 and adding shadow, `sketch(im, style = 2, shadow = 0.4)`,
the problems are largely solved. In addition, by setting the `smooth`
parameter to 0, `sketch(im, style = 2, lineweight = 1, smooth = 0,
shadow = 0.4)`, the sketch was successful in representing the fine
texture of bird
body.

<p>

<a href="https://raw.githubusercontent.com/tsuda16k/sketcher/master/notes/bird_1800.jpg"><img src="notes/bird_900.jpg" width="100%"></a>

</p>

### Case 2

Due to the lack of edges in the dark region of the face, the default
sketch, `sketch(im)`, produced a weird result. This can be fixed by
adding shadow, such as `sketch(im, shadow = 0.4)`. Finding the right
value of the shadow parameter usually requires trial and error, but
usually a value between 0.3 and 0.6 will produce a reasonable result.

<p>

<img src="notes/face3_900.jpg" width="80%">

</p>

### Case 3

The sketch algorithm detects edges in an image. If objects have unclear
edges/outlines, sketching will fail. This typically happens when a photo
contains 1) out of focus region or 2) objects with unclear outlines such
as furry animals.

A cat image below is such an example. The background of the photo and
the lower body of the cat is out of focus, and the outline of the cat is
blurry. The default sketch, `sketch(im)`, is unsuccessful in
representing the cat body. Zero smoothing, `sketch(im, smooth = 0)`, can
capture hairs/textures of the cat to some extent, at the cost of
increased noise in background, and still the body and legs of the cat is
not represented well. Unfortunately, there is no simple solution to the
difficulty with blurred/defocused images.

<p>

<img src="notes/cat_900.jpg" width="100%">

</p>

## Gallery

Here are some sketches produced by the sketcher package.

<p>

<img src="notes/gallery_cake.jpg" width="100%">

</p>

<p>

<img src="notes/gallery_container.jpg" width="100%">

</p>

<p>

<img src="notes/gallery_soba.jpg" width="100%">

</p>

<p>

<img src="notes/gallery_tokyo_st.jpg" width="100%">

</p>

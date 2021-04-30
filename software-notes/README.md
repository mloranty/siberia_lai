## Software Notes
This consists of some notes regarding the currently available software for calculation of LAI from hemispherical images.

#### [Hemiphoto.R](https://github.com/naturalis/Hemiphot)
* Takes in direct .jpg images and returned LAI values. 
* Seems to match up reasonably closely with Hemisfer. 
* Throws random errors on vertically aligned / "portrait" images, so I rotated LDF2_1_20140721.JPG,  LDF2_2_20140721.JPG, LDF2_3_20140721.JPG, LDF2_4_20140721.JPG by 90 deg to pass as input. 
* Outputs in hemiphot_output.txt - (...somewhat close to hemisfer outputs?)
* Last updated 2018. 

#### [Canopy Gap Analyzer](https://github.com/dabasler/CanopyGapAnalyzer)
* Python script to take in jpg images, binarize, and calculate LAI
* Documentation somewhat outdated, but main script seems to work. 
* Outputs at cga_output.txt - (...don't really seem very accurate?)

####  [MATLAB codes for canopy image analysis](https://www.mathworks.com/matlabcentral/fileexchange/24314-matlab-codes-for-canopy-image-analysis)
* Binarizes images and calculates canopy cover
* Tested to be working fine for what it is supposed to do, but last updated Aug 2009. 

#### [CIMES](http://jmnw.free.fr/)
* Detailed application with multiple scripts for multiple functions, good/updated documentation, etc. 
* However only takes a very specific 8-bit greyscale binary image in .bmp. From this it generates gap fraction data and performs calculations on those. 
* For testing, the .bmp can be generated quite easily through Photoshop - however it requires information to be manually entered (eg. coordinates of points on horizon image circle)

#### [hemiphoto2LAI](https://github.com/zhaokg/hemiphoto2LAI)
* Takes in gap fraction data to return LAI values. Does not take in images. 
* Last updated 2019.

#### [Canopy Image Analysis](https://github.com/GastonMauroDiaz/caiman)
* Package seems to be updated and maintained but I could not get it to work, aside from producing binarized images. 
* Outdated documentation with missing parameter values and such. 
* It is supposed to return binarized images and gap fraction data that can then be fed into other software like CIMES.


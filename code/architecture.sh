#!/bin/bash

# Clone the WhatWeb repository
git clone https://github.com/urbanadventurer/WhatWeb.git

# Change directory to the WhatWeb directory
cd WhatWeb

# Run the specified command
./whatweb -v https://www.epicov.org/epi3/frontend/ -a 1
./whatweb -v https://www.covid19dataportal.org/ -a 1

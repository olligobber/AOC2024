# Set up the folder and inputs for a day of Advent Of Code

# The first argument if provided is the day to set up, and defaults to the current day in local time
# The second argument if provided is the year to set up, and defaults to the current year in local time

# The session cookie for adventofcode.com should be located in ./session_cookie
# Go to "developer tools -> application -> cookies" in a logged in browser to get it

if [ $# -gt 0 ]; then 
	today=$(date -d "December $1" +%d)
	trimday=$(date -d "December $1" +%-d)
else 
	today=$(date +%d)
	trimday=$(date +%-d)
fi

if [ $# -gt 1 ]; then 
	year=$2
else 
	year=$(date +%Y)
fi 

# This won't work well if your local time zone isn't a whole number of hours plus or minus UTC
localunlocktime=$(date -d 'TZ="EST" 12am' +%-l%P)

echo "Setting up day $today for year $year"

# Check if the directory already exists
if [ -d "Day$today" ]; then
	echo "  Directory for day $today already exists!"
else
	# Make the directory
	mkdir "Day$today"
fi

# Go into the directory
cd "Day$today"

# Copy over the template files
if [ -f "Day$today.cabal" ]; then
	echo "  Cabal file already exists!"
else
	cat "../Template/Template.cabal" | sed "s/Day/Day$today/g" | sed "s/Year/$year/g" > "Day$today.cabal"
fi
if [ -f "Part1.hs" ]; then
	echo "  Source for part 1 already exists!"
else
	cp "../Template/Template.hs" "Part1.hs"
fi
if [ -f "Part2.hs" ]; then
	echo "  Source for part 2 already exists!"
else
	cp "../Template/Template.hs" "Part2.hs"
fi

echo "Starting cabal build, this may take a moment"

# Build cabal
cabal build -v0

echo "Cabal build complete"

# Check if input exists
if [ -f "input" ]; then
	echo "  Input data already exists!"
	exit 0
fi

# Check if session cookie is set up
if [ ! -f "../session_cookie" ]; then
	echo "Session cookie is missing, could not download input file."
	exit 1
fi

# Download the input file
cookie=$(cat ../session_cookie)
timeuntil=$(($(date -d "TZ=\"EST\" December $today $year 12am" +%s) - $(date +%s)))
if [ $timeuntil -gt 0 ]; then
	echo "Waiting until $localunlocktime to download input"
	sleep "$timeuntil"s
fi
echo "Downloading input now"
wget -q --header "Cookie: session=$cookie" "https://adventofcode.com/$year/day/$trimday/input"

# Report success/failure
if [ -f "input" ]; then
	echo "Download complete"
	exit 0
else
	echo "Download failed"
	exit 1
fi
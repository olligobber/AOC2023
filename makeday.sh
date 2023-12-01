# Set up the folder and inputs for a day of Advent Of Code
# The session cookie for adventofcode.com should be located in ./session_cookie
# Go to "developer tools -> application -> cookies" in a logged in browser to get it

# This won't work well if you run this before midnight and AOC starts at or after midnight in your timezone
today=$(date -d "today" +%d)
trimday=$(date -d "today" +%-d)

# This won't work well if your local time zone isn't a whole number of hours plus or minus UTC
localunlocktime=$(date -d 'TZ="EST" 12am' +%-l%P)

# Check if the directory already exists
if [ -d "Day$today" ]; then
	echo "Directory for day $today already exists!"
	exit 1
fi

echo "Setting up day $today"

# Make the directory
mkdir "Day$today"

# Copy over the template files
sed "s/Day/Day$today/g" < "Template/Template.cabal" > "Day$today/Day$today.cabal"
mkdir "Day$today/app"
cp "Template/Template.hs" "Day$today/app/Part1.hs"
cp "Template/Template.hs" "Day$today/app/Part2.hs"

# Go into the directory
cd "Day$today"

echo "Starting cabal build, this may take a moment"

# Build cabal
cabal build -v0

echo "Cabal build complete"

# Check if session cookie is set up
if [ ! -f "../session_cookie" ]; then
	echo "Session cookie is missing, could not download input file."
	exit 1
fi

# Download the input file
cookie=$(cat ../session_cookie)
timeuntil=$(expr `date -d "TZ=\"EST\" December $today 12am" +%s` - `date +%s`)
if [ $timeuntil -gt 0 ]; then
	echo "Waiting until $localunlocktime to download input"
	sleep "$timeuntil"s
fi
echo "Downloading input now"
wget -q --header "Cookie: session=$cookie" "https://adventofcode.com/2023/day/$trimday/input"

# Report success/failure
if [ -f "input" ]; then
	echo "Download complete"
	exit 0
else
	echo "Download failed"
	exit 1
fi
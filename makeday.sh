# Set up the folder and inputs for a day of AOC
# the session cookie for AOC should be located in ./session_cookie
# go to "developer tools -> application -> cookies" to get it

today=$(date -d "today" +%d)

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

# Check if session cookie is downloaded
if [ ! -f "../session_cookie" ]; then
    echo "Session cookie is missing, could not download input file."
    exit 1
fi

# Download the input file
cookie=$(cat ../session_cookie)
trimday=$(echo "$today" | sed -E "s/^0+//")
timeuntil=$(expr `date -d "today 4pm" +%s` - `date -d "now" +%s`)
if [ $timeuntil -gt 0 ]; then
    echo "Waiting until 4pm to download input"
    sleep "$timeuntil"s
fi
echo "Downloading input now"
wget -q --header "Cookie: session=$cookie" "https://adventofcode.com/2023/day/$trimday/input"
if [ -f "input" ]; then
    echo "Download complete"
    exit 0
else
    echo "Download failed"
    exit 1
fi
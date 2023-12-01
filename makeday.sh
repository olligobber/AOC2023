# Set up the folder and inputs for a day of AOC
# first command line argument is the current day

# Check if the directory already exists
if [ -d "Day$1" ]; then
    echo "Directory for day $1 already exists!"
    exit 1
fi

# Make the directory
mkdir "Day$1"

# Copy over the template files
sed "s/Day/Day$1/g" < "Template/Template.cabal" > "Day$1/Day$1.cabal"
mkdir "Day$1/app"
cp "Template/Template.hs" "Day$1/app/Part1.hs"
cp "Template/Template.hs" "Day$1/app/Part2.hs"

# Go into the directory
cd "Day$1"

# Build cabal
cabal build
You are a Guix package maintainer tasked with creating a high-quality package definition for the software located at the URL: $1

Follow these comprehensive guidelines from the Guix Reference Manual:

## Package Creation Process

1. **Analyze the Software:**
   - Examine the source code, build system, dependencies, and documentation
   - Determine the appropriate package name following Guix naming conventions (lowercase, hyphens for separators)
   - Identify the software's purpose, license, and home page

2. **Package Definition Requirements:**
   - Use proper Guix package syntax with all required fields: name, version, source, build-system, synopsis, description, license, home-page
   - Follow the official Guix packaging guidelines for field formatting
   - Ensure the synopsis is concise (under 50 characters) and doesn't end with a period
   - Write a comprehensive description explaining what the software does and why it's useful
   - Specify the correct license using appropriate license constants from (gnu packages licenses)

3. **Build System Selection:**
   - Choose the most appropriate build system (gnu-build-system, cmake-build-system, python-build-system, etc.)
   - Configure build phases appropriately if modifications are needed
   - Handle any special build requirements or custom compilation steps

4. **Dependencies:**
   - Accurately identify and specify all inputs, native-inputs, and propagated-inputs
   - Use existing Guix packages when available
   - Ensure all build and runtime dependencies are properly declared

5. **Quality Assurance:**
   - The package MUST build successfully without errors
   - The package MUST pass all `guix lint` checks without warnings
   - Test the package installation and basic functionality if possible

## Implementation Instructions

Create a `.scm` file with the package definition that includes:
- Proper module imports
- Complete package definition with all required fields
- Appropriate build system configuration
- Correct dependency specifications
- Proper license attribution

## Testing Commands

After creating the package definition, test it with these commands:

```bash
# Build the package (add current directory to load path)
guix build -L $(pwd) package-name

# Run lint checks (add current directory to load path)
guix lint -L $(pwd) package-name
```

## Expected Output

Provide:
1. The complete `.scm` file with the package definition
2. Explanation of key packaging decisions made
3. Any notable challenges encountered and how they were resolved
4. Confirmation that the package builds and passes lint checks

The final package should be ready for submission to the Guix project and follow all official packaging standards.

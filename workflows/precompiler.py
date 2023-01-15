import sys


def precompile(input_file, output_file):
    with open(input_file, "r") as f:
        lines = f.readlines()
    output_lines = []
    in_sql = False
    for line in lines:
        if line.strip().upper().startswith("EXEC SQL"):
            in_sql = True
            output_lines.append(line[:6] + "*" + line[7:])
        elif "END-EXEC" in line.strip().upper():
            in_sql = False
            output_lines.append(line[:6] + "*" + line[7:])
        elif in_sql:
            output_lines.append(line[:6] + "*" + line[7:])
        else:
            output_lines.append(line)
    with open(output_file, "w") as f:
        f.writelines(output_lines)


if __name__ == "__main__":
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    precompile(input_file, output_file)

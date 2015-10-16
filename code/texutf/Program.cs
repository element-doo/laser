using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace texutf
{
    class FileBody
    {
        public readonly string Path;
        public readonly string Body;

        public FileBody(string path, string body)
        {
            Path = path;
            Body = body;
        }

        public static FileBody readFileBody(string path)
        {
            var file = new FileInfo(path);
            if (!file.Exists)
            {
                throw new ArgumentException("File: \"" + file + "\" does not exist!");
            }

            var bytes = File.ReadAllBytes(path);
            var body = Encoding.UTF8.GetString(bytes);

            {
                var checkBytes = Encoding.UTF8.GetBytes(body);
                if (!checkBytes.SequenceEqual(bytes))
                {
                    throw new ArgumentException("Could not decode \"" + file + "\" with UTF-8!");
                }
            }

            return new FileBody(path, body);
        }
    }

    class PatternReplacement
    {
        public readonly Regex Pattern;
        public readonly string Replacement;

        public PatternReplacement(string pattern, string replacement)
        {
            Pattern = new Regex(pattern, RegexOptions.Compiled | RegexOptions.CultureInvariant);
            Replacement = replacement;
        }
    }

    class Replacer
    {
        private static readonly Regex LineSplitter = new Regex(@"[\r\n]+");
        private static readonly Regex LineCleaner = new Regex(@"(^\s+|\s+$|#.*)");
        private static readonly Regex PatternSplitter = new Regex(@"\s*=>\s*");

        private static IEnumerable<PatternReplacement> parsePatternReplacements(string replacements)
        {
            var lines = LineSplitter.Split(replacements);
            foreach (var line in lines)
            {
                var cleaned = LineCleaner.Replace(line, string.Empty);
                if (cleaned != string.Empty)
                {
                    var pattern = PatternSplitter.Split(cleaned, 2);
                    if (pattern.Length == 2)
                    {
                        yield return new PatternReplacement(pattern[0], pattern[1]);
                    }
                }
            }
        }

        private readonly List<PatternReplacement> PatternReplacements;

        public Replacer(string replacements)
        {
            PatternReplacements = parsePatternReplacements(replacements).ToList();
        }

        public string replace(string body)
        {
            var current = body;
            foreach (var patternReplacement in PatternReplacements)
            {
                current = patternReplacement.Pattern.Replace(current, patternReplacement.Replacement);
            }
            return current;
        }
    }

    class Program
    {
        static int Main(string[] args)
        {
            var executablePath = System.Reflection.Assembly.GetExecutingAssembly().CodeBase;
            var programName = Path.GetFileNameWithoutExtension(executablePath);

            if (args.Length == 0)
            {
                Console.Error.WriteLine("Usage: " + programName + " [file1.tex] [file2.tex] ...");
                return 1;
            }

            var exePattern = new Regex(@"^file:///(.*\.)exe$");
            var replacementsPath = exePattern.Replace(executablePath, "$1patterns");
            if (executablePath == replacementsPath)
            {
                Console.Error.WriteLine("Could not get executable path from: " + executablePath);
                return 2;
            }

            if (!File.Exists(replacementsPath))
            {
                Console.Error.WriteLine("Could not read patterns from: " + replacementsPath);
                return 3;
            }

            try
            {
                var replacementsFileBody = FileBody.readFileBody(replacementsPath);
                var replacer = new Replacer(replacementsFileBody.Body);

                foreach (var path in args)
                {
                    var fileBody = FileBody.readFileBody(path);
                    var original = fileBody.Body;
                    var target = replacer.replace(original);
                    if (original == target)
                    {
                        Console.WriteLine("No changes in file: " + path);
                    }
                    else
                    {
                        Console.Write("Replacing: " + path + " ...");
                        File.WriteAllText(path + ".bak", original, Encoding.UTF8);
                        File.WriteAllText(path, target, Encoding.UTF8);
                        Console.WriteLine("done!");
                    }
                }
                
                return 0;
            }
            catch (Exception e)
            {
                Console.Error.WriteLine(e.Message);
                return 4;
            }
        }
    }
}

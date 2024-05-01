using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using LanguageExt.TypeClasses;
using static LanguageExt.Prelude;

namespace LanguageExt.Sys.Test
{
    public delegate HashSet<EqFsEntry, FsEntry> FsMutation(HashSet<EqFsEntry, FsEntry> fs);

    /// <summary>
    /// 
    /// </summary>
    public class ImmutableFileSystem
    {
        // TODO
        // append file

        /// <summary>
        /// Creates a new empty immutable file system.
        /// </summary>
        /// <returns></returns>
        public static HashSet<EqFsEntry, FsEntry> NewFS() => HashSet<EqFsEntry, FsEntry>.Empty;

        /// <summary>
        /// Creates a new immutable file system with the given collection of root entries.
        /// </summary>
        /// <param name="rootEntries"></param>
        /// <returns></returns>
        public static HashSet<EqFsEntry, FsEntry> NewFS(IEnumerable<FsEntry> rootEntries) => new HashSet<EqFsEntry, FsEntry>(rootEntries);

        /// <summary>
        /// Creates a new immutable file system with the given collection of root entries.
        /// </summary>
        /// <param name="rootEntries"></param>
        /// <returns></returns>
        public static HashSet<EqFsEntry, FsEntry> NewFS(params FsEntry[] rootEntries) => new HashSet<EqFsEntry, FsEntry>(rootEntries);

        /// <summary>
        /// Creates a new empty immutable file system and immediately applies the given action sequence
        /// to it.
        /// </summary>
        /// <param name="actions"></param>
        /// <returns></returns>
        public static HashSet<EqFsEntry, FsEntry> NewFS(
            params Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>>[] actions
        ) => ApplyAll(NewFS(), actions.ToSeq());

        /// <summary>
        /// 
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="actions"></param>
        /// <returns></returns>
        public static HashSet<EqFsEntry, FsEntry> ApplyAll(
            HashSet<EqFsEntry, FsEntry> fs, 
            Seq<Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>>> actions
        ) =>
            actions.Fold(fs, (f, mut) => mut(f));

        /// <summary>
        /// Creates a new directory at the given path.
        /// Does nothing if the path is invalid or there
        /// is already a file system entry at the terminal
        /// node.
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> CreateDir(Seq<string> path) =>
            AppendEntry(path.Init, () => Dir.New(path.Last));

        /// <summary>
        /// Creates all directories in the given path if they do not exist.
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> EnsureDirPath(Seq<string> path) => 
            fs => DisjointPath(fs, path)
                .Match(
                    Empty: () => fs,
                    Seq: newPath => 
                        CreateNestedDirs(newPath)
                            .Map(newDir => path
                                .Take(path.Length - newPath.Length)
                                .Apply(existingPath => AppendEntry(existingPath, () => newDir))
                                .Invoke(fs)
                            )
                            .IfNone(fs)
                );

        /// <summary>
        /// Returns the parts of the path that do not exist from the root of the file system.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        /// <exception cref="NotImplementedException"></exception>
        public static Seq<string> DisjointPath(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            IntersectPath(fs, path)
                .Apply(existing => path.Skip(existing.Length));

        /// <summary>
        /// 
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Seq<string> IntersectPath(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            Walk(
                    fs, path, Seq<string>(),
                    (existing, entry) => entry.Name.Cons(existing),
                    (parent, _) => parent.IsSome
                )
                .Rev();

        /// <summary>
        /// Writes an empty file at the given path.
        /// Creates a new file if
        /// none exists or overwrites any existing file. Does nothing if the
        /// path is invalid.
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> WriteFile(Seq<string> path) => 
            WriteFile(path, string.Empty);

        /// <summary>
        /// Writes data to a file at the given path. Creates a new file if
        /// none exists or overwrites any existing file. Does nothing if the
        /// path is invalid.
        /// </summary>
        /// <param name="path"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> WriteFile(
            Seq<string> path, 
            string data
        ) =>
            AppendEntry(path.Init, () => File.New(path.Last, data));

        /// <summary>
        /// Appends data to the given file.
        /// The file will be created if it does not exist.
        /// Nothing will happen if the directory path is not valid.
        /// </summary>
        /// <param name="path"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> AppendFile(
            Seq<string> path,
            string data
        ) =>
            fs =>
                FindFile(fs, path)
                    .Match(
                        file => file.Data, 
                        () => string.Empty
                    )
                    .Apply(fileContents => WriteFile(path, fileContents + data))
                    .Invoke(fs);

        /// <summary>
        /// Mounts another file system as a new directory in the given path.
        /// </summary>
        /// <param name="otherFs"></param>
        /// <param name="path"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> Mount(
            HashSet<EqFsEntry, FsEntry> otherFs,
            Seq<string> path,
            string name
        ) =>
            AppendEntry(path, () => Dir.New(name, otherFs));

        /// <summary>
        /// <inheritdoc cref="Unmount"/>
        /// 
        /// <para>Synonym for <see cref="Unmount"/>.</para>
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> Delete(
            Seq<string> path
        ) => Unmount(path);

        /// <summary>
        /// Removes the entry at the given path. Does nothing if the entry does not exist.
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> Unmount(
            Seq<string> path
        ) =>
            fs => path
                .Match(
                    Empty: () => fs,
                    One: name => fs.Remove(BareEntry.New(name)),
                    More: (_, _) => CollectEntries(fs, path)
                        .Bind(AsDirs)
                        .Map(dirs => fs.SetItem(
                            dirs.Init.FoldBack(
                                dirs.Last.RemoveChild(path.Last),
                                (child, parent) => parent.SetChild(child)
                            ))
                        )
                        .IfNone(fs)
                );

        /// <summary>
        /// Attempts to read the file at the given path. Returns None if the path
        /// is invalid or the entry is not a file.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Option<string> ReadAllText(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            FindEntry(fs, path)
                .Bind(AsFile)
                .Map(file => file.Data);

        /// <summary>
        /// Updates the entry at the given path if it exists.
        /// </summary>
        /// <param name="path">
        /// Path to the parent where the new entry will be appended.
        /// </param>
        /// <param name="new"></param>
        /// <returns></returns>
        public static Func<HashSet<EqFsEntry, FsEntry>, HashSet<EqFsEntry, FsEntry>> AppendEntry(
            Seq<string> path, 
            Func<FsEntry> @new
        ) =>
            fs => CollectEntries(fs, path)
                .Bind(AsDirs)
                .Map(dirs => fs.AddOrUpdate(
                    dirs.FoldBack(
                        @new(),
                        (child, parent) => parent.SetChild(child)
                    )
                ))
                .IfNone(fs);

        /// <summary>
        /// Attempts to find a child entry by the given name which is a directory.
        /// </summary>
        /// <param name="parent"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        public static Option<Dir> FindChildDir(Dir parent, string name) =>
            FindChild(parent, name)
                .Bind(AsDir);

        /// <summary>
        /// Attempts to find a child entry by the given name which is a file.
        /// </summary>
        /// <param name="parent"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        public static Option<File> FindChildFile(Dir parent, string name) =>
            FindChild(parent, name)
                .Bind(entry => Optional(entry as File));

        /// <summary>
        /// Attempts to find a child entry by the given name.
        /// </summary>
        /// <param name="parent"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        public static Option<FsEntry> FindChild(FsEntry parent, string name) =>
            AsDir(parent)
                .Bind(dir => FindChild(dir, name));

        /// <summary>
        /// Attempts to find a child entry by the given name.
        /// </summary>
        /// <param name="parent"></param>
        /// <param name="name"></param>
        /// <returns></returns>
        public static Option<FsEntry> FindChild(Dir parent, string name) =>
            parent.Children.Find(BareEntry.New(name));

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <param name="initialState"></param>
        /// <param name="f"></param>
        /// <returns></returns>
        public static A Walk<A>(
            HashSet<EqFsEntry, FsEntry> fs,
            Seq<string> path,
            A initialState,
            Func<A, FsEntry, A> f
        ) => Walk(fs, path, initialState, f, (_, _) => true);

        /// <summary>
        /// 
        /// </summary>
        /// <typeparam name="A"></typeparam>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <param name="initialState"></param>
        /// <param name="f"></param>
        /// <param name="pred"></param>
        /// <returns></returns>
        public static A Walk<A>(
            HashSet<EqFsEntry, FsEntry> fs,
            Seq<string> path,
            A initialState,
            Func<A, FsEntry, A> f,
            Func<Option<FsEntry>, A, bool> pred
        ) =>
            path
                .FoldWhile(
                    (CurrentEntry: Some(Dir.Rooted(fs) as FsEntry), Value: initialState),
                    (state, name) =>
                        (
                            from entry in state.CurrentEntry
                            from child in FindChild(entry, name)
                            select child
                        )
                        .Match(
                            child => (Some(child), f(state.Value, child)),
                            () => (None, state.Value)
                        ),
                    state => pred(state.CurrentEntry, state.Value)
                )
                .Value;

        /// <summary>
        /// Attempts to collect all file system entries along the given
        /// path. Returns None if the path could not be resolved.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Option<Seq<FsEntry>> CollectEntries(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            Walk(
                fs,
                path,
                Seq<FsEntry>(),
                (entries, entry) => entry.Cons(entries)
            )
            .Rev()
            .Apply(Some)
            .Filter(entries => entries.Length == path.Length);

        /// <summary>
        /// Attempts to find the directory in the given file system located at the given path.
        /// Will return None if any part of the path cannot be resolved or if the entry
        /// at the end of the path is not a directory.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Option<Dir> FindDir(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            FindEntry(fs, path)
                .Bind(AsDir);

        /// <summary>
        /// Attempts to find the file in the given file system located at the given path.
        /// Will return None if any part of the path cannot be resolved or if the entry
        /// at the end of the path is not a file.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Option<File> FindFile(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            FindEntry(fs, path)
                .Bind(AsFile);

        /// <summary>
        /// Attempts to find the entry in the given file system located at the given path.
        /// </summary>
        /// <param name="fs">
        /// The file system to search.
        /// </param>
        /// <param name="path">
        /// Path to the entry within the file system.
        /// </param>
        /// <returns></returns>
        public static Option<FsEntry> FindEntry(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            path
                .Fold(
                    Some(Dir.Rooted(fs) as FsEntry),
                    (entry, name) => entry.Bind(e => FindChild(e, name))
                );

        /// <summary>
        /// Returns all child entries in the given path.
        /// Returns Empty if the path is invalid or not a directory.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Seq<FsEntry> EnumerateEntries(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            FindEntry(fs, path)
                .Bind(AsDir)
                .Map(entry => entry.Children.ToSeq())
                .IfNone(Empty);

        /// <summary>
        /// Returns all child files in the given path.
        /// Returns Empty if the path is invalid or not a directory.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Seq<File> EnumerateFiles(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            EnumerateEntries(fs, path)
                .Map(AsFile)
                .Somes();

        /// <summary>
        /// Returns all child directories in the given path.
        /// Returns Empty if the path is invalid or not a directory.
        /// </summary>
        /// <param name="fs"></param>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Seq<Dir> EnumerateDirectories(HashSet<EqFsEntry, FsEntry> fs, Seq<string> path) =>
            EnumerateEntries(fs, path)
                .Map(AsDir)
                .Somes();

        /// <summary>
        /// Attempts to treat the file system entry as a directory.
        /// Returns None if the entry was not a directory.
        /// </summary>
        /// <param name="entry"></param>
        /// <returns></returns>
        public static Option<Dir> AsDir(FsEntry entry) => Optional(entry as Dir);

        /// <summary>
        /// Attempts to treat the sequence of file system entries as directories.
        /// Returns None if any entry was not a directory.
        /// </summary>
        /// <param name="entries"></param>
        /// <returns></returns>
        public static Option<Seq<Dir>> AsDirs(Seq<FsEntry> entries) =>
            entries
                .Map(AsDir)
                .Traverse(identity);

        /// <summary>
        /// Attempts to treat the file system entry as a file.
        /// Returns None if the entry was not a file.
        /// </summary>
        /// <param name="entry"></param>
        /// <returns></returns>
        public static Option<File> AsFile(FsEntry entry) => Optional(entry as File);

        /// <summary>
        /// 
        /// </summary>
        /// <param name="path"></param>
        /// <returns></returns>
        public static Option<Dir> CreateNestedDirs(Seq<string> path) =>
            path
                .FoldBack(
                    Option<Dir>.None,
                    (child, name) => Dir.New(name, toHashSet<EqFsEntry, FsEntry>(child))
                );

    }

    public interface FsEntry
    {
        string Name { get; }
    }

    public record BareEntry : FsEntry
    {
        public string Name { get; }

        private BareEntry(string name) => Name = name;

        public static BareEntry New(string name) => new BareEntry(name);
    }

    public record Dir : FsEntry
    {
        public string Name { get; }
        public HashSet<EqFsEntry, FsEntry> Children { get; private set; }

        private Dir(string name, HashSet<EqFsEntry, FsEntry> children)
        {
            Name = name;
            Children = children;
        }

        public static Dir New(string name) => New(name, HashSet<EqFsEntry, FsEntry>.Empty);
        public static Dir New(string name, HashSet<EqFsEntry, FsEntry> children) => new Dir(name, children);

        /// <summary>
        /// Creates a new root directory.
        /// </summary>
        /// <param name="fs"></param>
        /// <returns></returns>
        public static Dir Rooted(HashSet<EqFsEntry, FsEntry> fs) => new Dir(string.Empty, fs);

        /// <summary>
        /// Returns a new directory with the new child.
        /// Overwrites existing entry, if present.
        /// </summary>
        /// <param name="child"></param>
        /// <returns></returns>
        public Dir SetChild(FsEntry child) =>
            this with
            {
                Children = Children.AddOrUpdate(child),
            };

        public Dir RemoveChild(string name) =>
            this with
            {
                Children = Children.Remove(BareEntry.New(name)),
            };

        /// <summary>
        /// Every Dir is itself a file system.
        /// </summary>
        /// <param name="dir"></param>
        public static implicit operator HashSet<EqFsEntry, FsEntry>(Dir dir) => dir.Children;
    }

    public record File : FsEntry
    {
        public string Name { get; }
        public string Data { get; }

        private File(string name, string data)
        {
            Name = name;
            Data = data;
        }

        public static File New(string name) => New(name, string.Empty);
        public static File New(string name, string data) => new File(name, data);
    }

    public struct EqFsEntry : Eq<FsEntry>
    {
        public Task<int> GetHashCodeAsync(FsEntry x) => Task.FromResult(GetHashCode(x));

        public int GetHashCode(FsEntry x) => x.Name.GetHashCode();

        public Task<bool> EqualsAsync(FsEntry x, FsEntry y) => Task.FromResult(Equals(x, y));

        public bool Equals(FsEntry x, FsEntry y) => string.Equals(x.Name, y.Name);
    }
}

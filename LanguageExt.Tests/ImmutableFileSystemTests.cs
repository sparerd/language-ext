using FluentAssertions;
using LanguageExt.Sys.Test;
using Xunit;
using static LanguageExt.Prelude;
using static LanguageExt.Sys.Test.ImmutableFileSystem;

namespace LanguageExt.Tests
{
    public class ImmutableFileSystemTests
    {
        [Fact]
        public void FindEntry_NoneWhenNotFound() => 
            FindEntry(NewFS(), Seq("a", "b"))
                .IsNone
                .Should().BeTrue();

        [Fact]
        public void FindEntry_SomeWhenFound() =>
            NewFS(
                    CreateDir(Seq1("a")),
                    WriteFile(Seq("a", "b"))
                )
                .Apply(fs => FindEntry(fs, Seq("a", "b")))
                .Case
                .Should().BeEquivalentTo(File.New("b"));

        [Fact]
        public void CollectEntries_NoneWhenPathNotResolved() =>
            CollectEntries(NewFS(), Seq1("a"))
                .IsNone.Should().BeTrue();

        [Fact]
        public void CollectEntries_EmptyWhenPathEmpty() =>
            CollectEntries(
                    HashSet<EqFsEntry, FsEntry>.Empty.Add(Dir.New("a")),
                    Seq<string>.Empty)
                .Case
                .Should().BeEquivalentTo(Seq<FsEntry>());

        [Fact]
        public void CollectEntries_ReturnsAllEntriesForValidPath() =>
            CollectEntries(
                    NewFS().Add(
                        Dir.New("a").SetChild(
                            Dir.New("b").SetChild(
                                Dir.New("c")))
                    ),
                    Seq("a", "b", "c")
                )
                .Case
                .Should().BeEquivalentTo(Seq(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c"))),
                    Dir.New("b").SetChild(
                        Dir.New("c")),
                    Dir.New("c")
                ));

        [Fact]
        public void CreateDir_AddsNewDir() =>
            NewFS()
                .Apply(CreateDir(Seq1("a")))
                .ToSeq()
                .Should().BeEquivalentTo(Seq1(Dir.New("a")));

        [Fact]
        public void CreateDir_CreatesDirAtPath() =>
            NewFS()
                .Add(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c"))
                    )
                )
                .Apply(CreateDir(Seq("a", "b", "c", "d")))
                .ToSeq()
                .Should().BeEquivalentTo(Seq1(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c").SetChild(
                                Dir.New("d")))
                    )
                ));

        [Fact]
        public void CreateDir_DoesNothingIfDirAlreadyPresent() =>
            NewFS()
                .Add(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c"))
                    )
                )
                .Apply(CreateDir(Seq("a", "b")))
                .ToSeq()
                .Should().BeEquivalentTo(Seq1(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c").SetChild(
                                Dir.New("d")))
                    )
                ));

        [Fact]
        public void WriteFile_WritesFileAtPath() =>
            NewFS()
                .Apply(WriteFile(Seq1("a"), "hello, world"))
                .ToSeq().Should().BeEquivalentTo(Seq1(File.New("a", "hello, world")));

        [Fact]
        public void AppendFile_CreatesFileIfNotExists() =>
            NewFS()
                .Apply(AppendFile(Seq1("a"), "hello, world"))
                .ToSeq().Should().BeEquivalentTo(Seq1(File.New("a", "hello, world")));

        [Fact]
        public void AppendFile_AppendsNewDataIfFileExists() =>
            NewFS(File.New("a", "123"))
                .Apply(AppendFile(Seq1("a"), "456"))
                .ToSeq().Should().BeEquivalentTo(Seq1(File.New("a", "123456")));

        [Fact]
        public void AppendFile_DoesNothingIfDirPathInvalid() =>
            NewFS()
                .Apply(AppendFile(Seq("a", "f1"), "456"))
                .ToSeq().Should().BeEmpty();

        [Fact]
        public void ReadAllText_SomeWhenFound() =>
            NewFS(WriteFile(Seq1("a"), "hello, world"))
                .Apply(fs => ReadAllText(fs, Seq1("a")))
                .Case.Should().Be("hello, world");

        [Fact]
        public void ReadAllText_NoneWhenNotFound() =>
            NewFS(WriteFile(Seq1("a"), "hello, world"))
                .Apply(fs => ReadAllText(fs, Seq1("b")))
                .IsNone.Should().BeTrue();

        [Fact]
        public void ApplyAll_Composes()
        {
            var action1 = CreateDir(Seq1("a"));
            var action2 = CreateDir(Seq("a", "b"));

            ApplyAll(
                NewFS(),
                Seq(action1, action2)
            )
            .Should().BeEquivalentTo(
                action2(action1(NewFS()))
            );
        }

        [Fact]
        public void NewFS_WithActions_Composes()
        {
            var action1 = CreateDir(Seq1("a"));
            var action2 = CreateDir(Seq("a", "b"));
            var action3 = CreateDir(Seq("a", "b", "c"));

            NewFS(action1, action2, action3)
                .Should().BeEquivalentTo(
                    action3(action2(action1(NewFS())))
                );
        }

        [Fact]
        public void Walk_ReturnsStateFromValidPortionOfWalk() =>
            NewFS(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c")))
                )
                .Apply(fs => Walk(fs,
                    Seq("a", "b", "d"),
                    Seq<string>(),
                    (names, entry) => names.Add(entry.Name)
                ))
                .Should().BeEquivalentTo(Seq("a", "b"));

        [Fact]
        public void Walk_RunsFnForEachValidStepInWalk() =>
            NewFS(
                CreateDir(Seq1("a")),
                CreateDir(Seq("a", "b")),
                CreateDir(Seq("a", "b", "c")),
                WriteFile(Seq("a", "b", "c", "f"))
            )
            .Apply(fs => Walk(fs,
                Seq("a", "b", "c", "f"),
                Seq<FsEntry>(),
                (entries, entry) => entries.Add(entry)
            ))
            .Should().BeEquivalentTo(Seq<FsEntry>(
                Dir.New("a"),
                Dir.New("b"),
                Dir.New("c"),
                File.New("f")
            ));

        [Fact]
        public void EnumerateEntries_ReturnsAllEntriesAtPath() =>
            NewFS(
                CreateDir(Seq1("a")),
                CreateDir(Seq("a", "b")),
                CreateDir(Seq("a", "b", "c")),
                WriteFile(Seq("a", "b", "d")),
                CreateDir(Seq("a", "b", "e")),
                WriteFile(Seq("a", "b", "f"))
            )
            .Apply(fs => EnumerateEntries(fs, Seq("a", "b")))
            .Should().BeEquivalentTo(Seq<FsEntry>(
                Dir.New("c"),
                File.New("d"),
                Dir.New("e"),
                File.New("f")
            ));

        [Fact]
        public void EnumerateEntries_EmptyIfPathInvalid() =>
            NewFS(
                    CreateDir(Seq1("a")),
                    CreateDir(Seq("a", "b")),
                    CreateDir(Seq("a", "b", "c"))
                )
                .Apply(fs => EnumerateEntries(fs, Seq("a", "x")))
                .Should().BeEmpty();

        [Fact]
        public void EnumerateFiles_ReturnsOnlyFilesAtPath() =>
            NewFS(
                    CreateDir(Seq1("a")),
                    CreateDir(Seq("a", "b")),
                    CreateDir(Seq("a", "b", "c")),
                    WriteFile(Seq("a", "b", "d")),
                    CreateDir(Seq("a", "b", "e")),
                    WriteFile(Seq("a", "b", "f"))
                )
                .Apply(fs => EnumerateFiles(fs, Seq("a", "b")))
                .Should().BeEquivalentTo(Seq<FsEntry>(
                    File.New("d"),
                    File.New("f")
                ));

        [Fact]
        public void EnumerateEntries_ReturnsOnlyDirsAtPath() =>
            NewFS(
                    CreateDir(Seq1("a")),
                    CreateDir(Seq("a", "b")),
                    CreateDir(Seq("a", "b", "c")),
                    WriteFile(Seq("a", "b", "d")),
                    CreateDir(Seq("a", "b", "e"))
                )
                .Apply(fs => EnumerateDirectories(fs, Seq("a", "b")))
                .Should().BeEquivalentTo(Seq<FsEntry>(
                    Dir.New("c"),
                    Dir.New("e")
                ));

        [Fact]
        public void CreateDirPath_CreatesOnlyPartsThatDontExist() =>
            NewFS()
                .Apply(EnsureDirPath(Seq("a", "b", "c", "d")))
                .Should().BeEquivalentTo(NewFS().Add(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c").SetChild(
                                Dir.New("d"))))
                ));

        [Fact]
        public void Mount_AttachesOtherFsInNewDirAtPath()
        {
            var fs1 = NewFS(
                Dir.New("a").SetChild(
                    Dir.New("b"))
            );

            var fs2 = NewFS(
                Dir.New("c").SetChild(
                    File.New("f1"))
            );

            fs1
                .Apply(Mount(fs2, Seq("a", "b"), "x"))
                .Should().BeEquivalentTo(NewFS(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("x").SetChild(
                                Dir.New("c").SetChild(
                                    File.New("f1")))))
                ));
        }

        [Fact]
        public void Unmount_EmptyPathDoesNothing() =>
            NewFS(Dir.New("a"))
                .Apply(Unmount(Empty))
                .Should().BeEquivalentTo(NewFS(Dir.New("a")));

        [Fact]
        public void Unmount_DoesNothingIfPathNotFound() =>
            NewFS(Dir.New("a"))
                .Apply(Unmount(Seq1("b")))
                .Should().BeEquivalentTo(NewFS(Dir.New("a")));

        [Fact]
        public void Unmount_RemovesTargetDir() =>
            NewFS(Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c")))
                )
                .Apply(Unmount(Seq("a", "b")))
                .Should().BeEquivalentTo(NewFS(Dir.New("a")));

        [Fact]
        public void CreateNested_NoneWhenPathEmpty() =>
            CreateNestedDirs(Empty)
                .IsNone.Should().BeTrue();

        [Fact]
        public void CreateNested_NestsChildrenInPathOrder() =>
            CreateNestedDirs(Seq("a", "b", "c"))
                .Case.Should().BeEquivalentTo(
                    Dir.New("a").SetChild(
                        Dir.New("b").SetChild(
                            Dir.New("c")))
                );

        [Fact]
        public void DisjointPath_ReturnsOnlyPartsNotPresentInFileSystem() =>
            NewFS(
                    Dir.New("a").SetChild(
                        Dir.New("b"))
                )
                .Apply(fs => DisjointPath(fs, Seq("a", "b", "c", "d")))
                .Should().BeEquivalentTo(Seq("c", "d"));

        [Fact]
        public void IntersectPath_ReturnsOnlyPartsNotPresentInFileSystem() =>
            NewFS(
                    Dir.New("a").SetChild(
                        Dir.New("b"))
                )
                .Apply(fs => IntersectPath(fs, Seq("a", "b", "c", "d")))
                .Should().BeEquivalentTo(Seq("a", "b"));
    }
}

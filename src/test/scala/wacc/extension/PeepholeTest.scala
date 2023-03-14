package wacc.extension

import wacc.backend._
import wacc.extension.peephole.Peephole

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import scala.collection.mutable


class PeepholeTest extends AnyFlatSpec {

    "replaceMovAddWithAdd" should "replace mov add with just add" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            Move(R2, ImmVal(1)),
            AddInstr(R3, R4, R2)
        )
        val expected = List(
            AddInstr(R3, R4, ImmVal(1))
        )
        assert(Peephole.replaceMovAddWithAdd()(instructions).equals(expected))
    }

    "replaceSubLdrWithLdr" should "replace sub ldr with ldr" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            SubInstr(R2, R3, ImmVal(20)),
            Load(R4, OffsetMode(R2, shiftAmount = ImmVal(0)))
        )
        val expected = List(
            Load(R4, OffsetMode(R3, shiftAmount = ImmVal(-20)))
        )
        assert(Peephole.replaceSubLdrWithLdr()(instructions).equals(expected))
    }

    "removeRedundantSubAndAdd" should "remove redundant sub and add" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            SubInstr(R2, R2, ImmVal(0)),
            AddInstr(R2, R2, ImmVal(0)), 
            SubInstr(SP, SP, ImmVal(0)),
            AddInstr(SP, SP, ImmVal(10))
        )

        val expected = List(
            AddInstr(SP, SP, ImmVal(10))
        )

        Peephole.removeRedundantSubAndAdd()(instructions) should equal(expected)
    }

    "removeRedundantMov" should "remove redundant mov" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            Move(R2, R2),
            Move(R2, R4)
        )

        val expected = List(
            Move(R2, R4)
        )

        Peephole.removeRedundantMov()(instructions) should equal(expected)
    }

    "removeRedundantLoad" should "remove redundant load instruction" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            Load(R0, ImmVal(32)),
            Load(R0, ImmVal(64))
        )
        val expected = List(
            Load(R0, ImmVal(64))
        )
        assert(Peephole.removeRedundantLoad()(instructions).equals(expected))
    }

    "removeRedundantStrLdr" should "remove redundant load/store instruction" in {
        val instructions1: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            Load(R0, R2),
            Store(R0, R2)
        )
        val instructions2 = instructions1.reverse
        val expected1 = List(
            Load(R0, R2)
        )
        val expected2 = List(
            Store(R0, R2)
        )
        assert(Peephole.removeRedundantStrLdr()(instructions1).equals(expected1))
        assert(Peephole.removeRedundantStrLdr()(instructions2).equals(expected2))
    }


    "replacePushMovPopWithMov" should "replace push mov pop with mov" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            Push(List(R8)), 
            Move(R0, R1),
            Pop(List(R8))
        )
        val expected = List(
            Move(R0, R1)
        )
        assert(Peephole.replacePushMovPopWithMov()(instructions).equals(expected))
    }

    "replacePushMovPopWithMov" should "not replace push mov pop with mov when mov uses same register as push" in {
        val instructions: mutable.ListBuffer[Instruction] = mutable.ListBuffer(
            Push(List(R8)), 
            Move(R8, R1),
            Pop(List(R8))
        )
        val expected = instructions
        assert(Peephole.replacePushMovPopWithMov()(instructions).equals(expected))
    }
}

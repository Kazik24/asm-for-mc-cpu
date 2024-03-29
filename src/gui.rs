
use druid::widget::*;
use druid::*;
use crate::vm::VirtualMachine;
use druid::lens::Map;
use num_traits::Pow;
use crate::emulator::{Opcode, Command, SubCommand, Mux2};


fn build_gui() -> impl Widget<GuiData> {
    let w = RamView::new().controller(RamControl);
    let tick_btn = Button::new("Tick").on_click(|_,data: &mut GuiData,_|{
        data.vm.tick(false);
    });
    let reset_cpu = Button::new("Reset CPU").on_click(|_,data: &mut GuiData,_|{
        data.vm.cpu_mut().reset();
    });
    let reset_ram = Button::new("Reset RAM").on_click(|_,data: &mut GuiData,_|{
        data.vm.reset_ram();
    });

    let left = Flex::column().main_axis_alignment(MainAxisAlignment::SpaceEvenly).cross_axis_alignment(CrossAxisAlignment::Start)
        .with_flex_child(w.expand(),1.0)
        .with_child(Flex::row().with_child(Label::dynamic(|d: &GuiData,_|{
            match d.vm.ram().get(d.vm.cpu().reg[15] as usize).copied().map(Opcode::from) {
                Some(op) => format!("Cell value: {:04X}  {}",op.bits(),op),
                None => format!("Cell value: -- ")
            }
        }).padding(5.0)));
    let right = Flex::column().main_axis_alignment(MainAxisAlignment::Start).cross_axis_alignment(CrossAxisAlignment::Start)
        .with_child(build_cpu_state_view().border(Color::BLUE,1.0))
        .with_flex_child(tick_btn.fix_width(150.0),1.0)
        .with_flex_child(reset_cpu.fix_width(150.0),1.0)
        .with_flex_child(reset_ram.fix_width(150.0),1.0);
    Flex::row().with_flex_child(left,1.0).with_child(right)
}

struct RamControl;
impl Controller<GuiData, RamView> for RamControl {
    fn event(&mut self, child: &mut RamView, _ctx: &mut EventCtx, event: &Event, data: &mut GuiData, _env: &Env) {
        match event {
            Event::Wheel(mouse) => {
                if mouse.wheel_delta.y > 0.0 {
                    data.ram_scroll = data.ram_scroll.saturating_add(child.prev_size.0 as _);
                }else if mouse.wheel_delta.y < 0.0 {
                    data.ram_scroll = data.ram_scroll.saturating_sub(child.prev_size.0 as _);
                }
            }
            _ => {}
        }
    }
}
struct RamView{
    prev_size: (i32,i32),
    prev_scroll: usize,
    view: WidgetPod<GuiData,Box<dyn Widget<GuiData>>>,
}

const W_CELL: f64 = 40.0;
const H_CELL: f64 = 24.0;
const COL_ADR_EVEN: Color = Color::GRAY;
const COL_ADR_ODD: Color = Color::MAROON;
const COL_EX_INSTR: Color = Color::from_rgba32_u32(0xa8a8a8ff);
const COL_RAM_INSTR: Color = Color::from_rgba32_u32(0x1FE03Cff);
const COL_RAM_READ: Color = Color::from_rgba32_u32(0x0022FFff);
const COL_RAM_WRITE: Color = Color::from_rgba32_u32(0xFFDD00ff);
const COL_CLDI_INSTR: Color = Color::from_rgba32_u32(0xE01FC3ff);
const COL_CALL_INSTR: Color = Color::from_rgba32_u32(0xE3881Cff);

fn hex_digits_for(value: usize)->usize{
    if value == 0 { return 1; }
    let value = value - 1;
    let mut cmp = 0xf;
    for i in 1..(usize::BITS/4) {
        if value <= cmp { return i as _;}
        cmp = (cmp << 4) | 0xf;
    }
    (usize::BITS/4) as _
}


impl RamView{
    pub fn new()->Self{
        let wg = Self::create_layout(8,10,0);
        //ViewSwitcher::new()
        Self{
            prev_size: (0,0),
            prev_scroll: 0,
            view: WidgetPod::new(wg.expand().background(Color::LIME)).boxed()
        }
    }

    fn label_for(index: usize,color: Color)->impl Widget<GuiData>{
        let label = Label::dynamic(move |data: &GuiData,_env|{
            data.vm.ram().get(index).map(|v|format!("{:04X}",v)).unwrap_or_else(||"--".to_string())
        });
        Container::new(Align::centered(label).fix_size(W_CELL,H_CELL).background(Painter::new(move |ctx, data: &GuiData, _env| {
            let cpu = data.vm.cpu();
            let pc = cpu.reg[15] as usize;
            let address = cpu.out_address as usize;
            let rect = ctx.size().to_rect();
            match cpu.decoded_opcode.cmd() {
                Command::Movw if pc != address => {
                    if index == pc.saturating_sub(1) {
                        ctx.fill(rect, &COL_RAM_INSTR);
                    } else if index == address {
                        if cpu.out_read {
                            ctx.fill(rect, &COL_RAM_READ);
                        } else if cpu.out_write {
                            ctx.fill(rect, &COL_RAM_WRITE);
                        }
                    }
                }
                Command::Ex if cpu.decoded_opcode.b_sub() == SubCommand::Cldi && (cpu.mux2 == Mux2::StateSaveToReg(false) || cpu.mux2 == Mux2::StateSkip) => {
                    if index == pc {
                        if cpu.mux2 == Mux2::StateSkip {
                            ctx.fill(rect, &COL_EX_INSTR);
                        }else{
                            ctx.fill(rect, &COL_RAM_READ);
                        }
                    }
                    if index == pc.saturating_sub(1) {
                        ctx.fill(rect, &COL_CLDI_INSTR);
                    }
                }
                Command::Ex if cpu.decoded_opcode.b_sub() == SubCommand::Call && (cpu.mux2 == Mux2::StateSaveToReg(true) || cpu.mux2 == Mux2::StateSkip) => {
                    if index == pc {
                        if cpu.mux2 == Mux2::StateSkip {
                            ctx.fill(rect, &COL_EX_INSTR);
                        }else{
                            ctx.fill(rect, &COL_RAM_READ);
                        }
                    }
                    if index == pc.saturating_sub(1) {
                        ctx.fill(rect, &COL_CALL_INSTR);
                    }
                }
                _ if pc == index => {
                    ctx.fill(rect,&COL_EX_INSTR);
                }
                _ => {}
            }
        }))).background(color)
    }
    fn create_layout(width:usize,height:usize,pos:usize)->impl Widget<GuiData>{

        let mut main = Flex::column();

        let mut top_row = Flex::row();
        top_row.add_child(SizedBox::empty().fix_size(W_CELL,H_CELL));
        let digits = hex_digits_for(width);
        for i in 0..width {
            top_row.add_child(Align::centered(Label::new(format!("{:0d$X}",i,d=digits)))
                .fix_size(W_CELL,H_CELL).background(if i % 2 == 0 { COL_ADR_EVEN } else { COL_ADR_ODD }));
        }
        main.add_child(top_row);
        let mut ram_index = (pos / width) * width;
        for y in 0..height {
            let mut row = Flex::row();
            row.add_child(Align::centered(Label::new(format!("{:04X}",ram_index)))
                .fix_size(W_CELL,H_CELL).background(if y % 2 == 0 { COL_ADR_EVEN } else { COL_ADR_ODD }));
            for _ in 0..width {
                row.add_child(Self::label_for(ram_index,Color::grey(0.1)));
                ram_index += 1;
            }
            main.add_child(row);
        }
        main
    }
    fn calc_size(size: Size)->(i32,i32){
        let width = ((size.width / W_CELL).floor() as i32 - 1).max(1);
        let height = ((size.height / H_CELL).floor() as i32 - 1).max(1);
        let width = 2.0.pow((width as f64).log2().floor()) as i32;
        (width,height)
    }
}
impl Widget<GuiData> for RamView{
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut GuiData, env: &Env) {
        self.view.event(ctx,event,data,env)
    }

    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &GuiData, env: &Env) {
        let new_size = Self::calc_size(ctx.size());
        match event {
            LifeCycle::WidgetAdded => {
                self.prev_size = new_size;
                self.prev_scroll = data.ram_scroll;
                let w = Self::create_layout(new_size.0 as _,new_size.1 as _,data.ram_scroll);
                self.view = WidgetPod::new(w).boxed();
                ctx.children_changed();
            }
            LifeCycle::Size(size) => {
                let new_size = Self::calc_size(*size);
                if new_size != self.prev_size || data.ram_scroll != self.prev_scroll{
                    self.prev_size = new_size;
                    self.prev_scroll = data.ram_scroll;
                    if new_size.0.count_ones() == 1 {
                        let w = Self::create_layout(new_size.0 as _,new_size.1 as _,data.ram_scroll);
                        self.view = WidgetPod::new(w).boxed();
                        self.view.lifecycle(ctx,&LifeCycle::WidgetAdded,data,env);
                        ctx.children_changed();
                        return;
                    }
                }
            }
            _ => {}
        }
        self.view.lifecycle(ctx,event,data,env)
    }

    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &GuiData, data: &GuiData, env: &Env) {
        let width = ((ctx.size().width / W_CELL).floor() as i32 - 1).max(1);
        let height = ((ctx.size().height / H_CELL).floor() as i32 - 1).max(1);
        let width = 2.0.pow((width as f64).log2().floor()) as i32;
        if old_data.ram_scroll != data.ram_scroll {
            let w = Self::create_layout(width as _,height as _,data.ram_scroll);
            self.view = WidgetPod::new(w).boxed();
            ctx.children_changed();
            return;
        }
        self.view.update(ctx,data,env)
    }

    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &GuiData, env: &Env) -> Size {

        self.view.layout(ctx,bc,data,env);
        bc.max()
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &GuiData, env: &Env) {
        let s = ctx.size();
        ctx.clip(s.to_rect());
        self.view.paint(ctx,data,env)
    }
}

#[derive(Clone,Eq,PartialEq)]
struct GuiData{
    vm: VirtualMachine,
    ram_scroll: usize,
}

impl Data for GuiData{
    fn same(&self, other: &Self) -> bool {
        self == other
    }
}


impl GuiData{
    pub fn new(vm: VirtualMachine)->Self{
        Self{ vm,ram_scroll:0 }
    }
}

pub fn launch_emulator_gui(vm: VirtualMachine){
    let main_window = WindowDesc::new(build_gui).title("Emulator view");
    let data = GuiData::new(vm);

    AppLauncher::with_window(main_window).launch(data).unwrap();
}

fn bits_view()->impl Widget<u16> {
    let lab = Label::new(|value: &u16, _env: &_| format!("{:04X}",value)).center().fix_width(W_CELL);
    const RECT_SIZE: f64 = 11.0;
    struct BitsControl;
    impl<W: Widget<u16>> Controller<u16, W> for BitsControl {
        fn event(&mut self, _child: &mut W, _ctx: &mut EventCtx, event: &Event, data: &mut u16, _env: &Env) {
            match event {
                Event::MouseUp(mouse) => {
                    for i in 0..=15 {
                        let r = Rect::new(i as f64 *RECT_SIZE,0.0,(i+1) as f64 *RECT_SIZE,RECT_SIZE);
                        if r.contains(mouse.pos) {
                            *data ^= 1 << (15-i);
                        }
                    }
                }
                _ => {}
            }
        }
    }
    let bits = Painter::new(move|ctx,data:&u16,_|{
        for i in 0..=15 {
            let bit = *data & (1<<(15-i)) != 0;
            let r = Rect::new(i as f64 *RECT_SIZE,0.0,(i+1) as f64 *RECT_SIZE,RECT_SIZE);
            ctx.fill(r,if bit {&Color::RED} else {&Color::YELLOW});
            ctx.stroke(r,&Color::BLUE,1.0);
        }
    }).fix_size(RECT_SIZE*16.0,RECT_SIZE).controller(BitsControl);


    Flex::row()
        .with_child(lab.padding(Insets{x0:10.0,x1:5.0,..Insets::default()}))
        .with_child(bits)
}
fn build_cpu_state_view() ->impl Widget<GuiData>{
    let mut labels = Flex::column();
    labels.add_child(Label::new("iv"));
    for i in 1..=14 {
        labels.add_child(Label::new(format!("r{}", i)).align_left())
    }
    labels.add_child(Label::new("r15/pc ").align_left());
    labels.set_cross_axis_alignment(CrossAxisAlignment::Start);
    let mut regs = Flex::column().with_child(bits_view().lens(
        Map::new(|_: &GuiData|0,|_: &mut GuiData,_|{})));
    for i in 1..=15 {
        let lens = Map::new(move |g: &GuiData|g.vm.cpu().reg[i],move |g: &mut GuiData,v|{g.vm.cpu_mut().reg[i] = v;});
        regs.add_child(bits_view().lens(lens));
    }
    Flex::row().with_child(labels).with_child(regs)
}